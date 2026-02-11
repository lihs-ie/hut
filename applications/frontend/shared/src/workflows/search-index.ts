import {
  AggregateNotFoundError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, combineAsync, ok, Result } from "@shared/aspects/result";
import { Logger } from "@shared/aspects/logger";
import { Scorer, generateNgrams } from "@shared/aspects/ngram";
import {
  Criteria,
  SearchIndex,
  UnvalidatedCriteria,
  extractReferences,
} from "@shared/domains/search-index/common";
import { Article, ArticleIdentifier } from "@shared/domains/articles";
import { Series, SeriesIdentifier } from "@shared/domains/series";
import { Memo, MemoIdentifier } from "@shared/domains/memo";
import { Command } from "./common";

type SearchByIndicesCommand = Command<UnvalidatedCriteria>;

export type SearchResult = Article | Memo | Series;

export type SearchIndexError =
  | ValidationError[]
  | UnexpectedError
  | AggregateNotFoundError<"Article">
  | AggregateNotFoundError<"Memo">
  | AggregateNotFoundError<"Series">;

export type SearchByIndexWorkflow = (
  command: SearchByIndicesCommand,
) => AsyncResult<SearchResult[], SearchIndexError>;

type ValidateCriteria = (
  unvalidated: UnvalidatedCriteria,
) => Result<Criteria, ValidationError[]>;

type GetIndices = (
  criteria: Criteria,
) => AsyncResult<SearchIndex[], UnexpectedError>;

type OfArticleIdentifiers = (
  identifiers: ArticleIdentifier[],
) => AsyncResult<
  Article[],
  UnexpectedError | AggregateNotFoundError<"Article">
>;

type OfMemoIdentifiers = (
  identifiers: MemoIdentifier[],
) => AsyncResult<Memo[], UnexpectedError | AggregateNotFoundError<"Memo">>;

type OfSeriesIdentifiers = (
  identifiers: SeriesIdentifier[],
) => AsyncResult<Series[], UnexpectedError | AggregateNotFoundError<"Series">>;

type ScoredResult = {
  item: SearchResult;
  score: number;
};

const scoreSearchIndex = (
  index: SearchIndex,
  freeWord: string | null,
  scorer: Scorer<"title" | "excerpt" | "tags">,
): number => {
  if (freeWord === null) {
    return 0;
  }

  const queryNgrams = generateNgrams(freeWord);
  const titleScore = scorer(queryNgrams, index.title, "title");
  const excerptScore = scorer(queryNgrams, index.excerpt, "excerpt");
  const tagsScore = index.tags.reduce(
    (accumulator, tag) => accumulator + scorer(queryNgrams, tag, "tags"),
    0,
  );

  return titleScore * 3 + excerptScore + tagsScore * 2;
};

const buildScoreMap = (
  indices: SearchIndex[],
  freeWord: string | null,
  scorer: Scorer<"title" | "excerpt" | "tags">,
): Map<string, number> => {
  const scoreMap = new Map<
    ArticleIdentifier | MemoIdentifier | SeriesIdentifier,
    number
  >();

  for (const index of indices) {
    const score = scoreSearchIndex(index, freeWord, scorer);
    scoreMap.set(index.reference, score);
  }

  return scoreMap;
};

const sortByScore = (results: ScoredResult[]): SearchResult[] => {
  return results
    .toSorted((a, b) => b.score - a.score)
    .map((result) => result.item);
};

export const createSearchByIndexWorkflow =
  (validateCriteria: ValidateCriteria) =>
  (logger: Logger) =>
  (scorer: Scorer<"title" | "excerpt" | "tags">) =>
  (getIndices: GetIndices) =>
  (ofArticleIdentifiers: OfArticleIdentifiers) =>
  (ofMemoIdentifiers: OfMemoIdentifiers) =>
  (ofSeriesIdentifiers: OfSeriesIdentifiers): SearchByIndexWorkflow =>
  (command: SearchByIndicesCommand) => {
    logger.info("SearchByIndexWorkflow started", {
      criteria: command.payload,
    });

    return validateCriteria(command.payload)
      .toAsync()
      .tap((criteria) => {
        logger.debug("Validation passed", { criteria });
      })
      .tapError((errors) => {
        logger.warn("Validation failed", { errors });
      })
      .andThen((criteria) =>
        getIndices(criteria).map((indices) => ({ criteria, indices })),
      )
      .tap(({ indices }) => {
        logger.debug("Indices retrieved", { count: indices.length });
      })
      .andThen(({ criteria, indices }) => {
        const [articleIdentifiers, memoIdentifiers, seriesIdentifiers] =
          extractReferences(indices);

        logger.debug("Identifiers extracted", {
          articles: articleIdentifiers.length,
          memos: memoIdentifiers.length,
          series: seriesIdentifiers.length,
        });

        const scoreMap = buildScoreMap(indices, criteria.freeWord, scorer);

        const articleResults =
          articleIdentifiers.length > 0
            ? ofArticleIdentifiers(articleIdentifiers)
            : ok<Article[], never>([]).toAsync();

        const memoResults =
          memoIdentifiers.length > 0
            ? ofMemoIdentifiers(memoIdentifiers)
            : ok<Memo[], never>([]).toAsync();

        const seriesResults =
          seriesIdentifiers.length > 0
            ? ofSeriesIdentifiers(seriesIdentifiers)
            : ok<Series[], never>([]).toAsync();

        return combineAsync([articleResults, memoResults, seriesResults]).map(
          ([articles, memos, series]) => {
            const allResults: SearchResult[] = [
              ...articles,
              ...memos,
              ...series,
            ];

            const scoredResults: ScoredResult[] = allResults.map((item) => ({
              item,
              score: scoreMap.get(item.identifier) ?? 0,
            }));

            return sortByScore(scoredResults);
          },
        );
      })
      .tap((results) => {
        logger.info("SearchByIndexWorkflow completed", {
          resultCount: results.length,
        });
      })
      .tapError((error) => {
        logger.error("SearchByIndexWorkflow failed", { error });
      });
  };
