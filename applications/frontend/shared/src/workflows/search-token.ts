import {
  AggregateNotFoundError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, ok, Result } from "@shared/aspects/result";
import { Logger } from "@shared/aspects/logger";
import { generateNgrams } from "@shared/aspects/ngram";
import {
  SearchTokenIdentifier,
  SearchReference,
  SearchToken,
  createSearchTokenIdentifier,
  SearchTokenType,
  searchTokenValueSchema,
  ContentType,
  Criteria,
  Sort,
  Order,
  UnvalidatedCriteria,
} from "@shared/domains/search-token";
import { Article, ArticleIdentifier } from "@shared/domains/articles";
import { Series, SeriesIdentifier } from "@shared/domains/series";
import { Memo, MemoIdentifier } from "@shared/domains/memo";
import { TagIdentifier } from "@shared/domains/attributes/tag";
import { Command } from "./common";

type SearchByTokensCommand = Command<UnvalidatedCriteria>;

export type SearchResult = Article | Memo | Series;

export type SearchTokenError =
  | ValidationError[]
  | UnexpectedError
  | AggregateNotFoundError<"Article">
  | AggregateNotFoundError<"Memo">
  | AggregateNotFoundError<"Series">
  | AggregateNotFoundError<"SearchToken">;

export type SearchByTokenWorkflow = (
  command: SearchByTokensCommand,
) => AsyncResult<SearchResult[], SearchTokenError>;

type ValidateCriteria = (
  unvalidated: UnvalidatedCriteria,
) => Result<Criteria, ValidationError[]>;

type OfTokenIdentifiers = (
  identifiers: SearchTokenIdentifier[],
  throwOnMissing?: boolean,
) => AsyncResult<
  SearchToken[],
  UnexpectedError | AggregateNotFoundError<"SearchToken">
>;

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

const generateNgramTokenIdentifiers = (
  freeWord: string,
): SearchTokenIdentifier[] => {
  const ngrams = generateNgrams(freeWord);
  return ngrams.map((ngram) => {
    const value = searchTokenValueSchema.parse(ngram);
    return createSearchTokenIdentifier(SearchTokenType.NGRAM, value);
  });
};

const generateTagTokenIdentifiers = (
  tags: TagIdentifier[],
): SearchTokenIdentifier[] => {
  return tags.map((tag) => {
    const value = searchTokenValueSchema.parse(tag);
    return createSearchTokenIdentifier(SearchTokenType.TAG, value);
  });
};

const createReferenceKey = (reference: SearchReference): string => {
  return `${reference.identifier.type}:${reference.identifier.content}`;
};

const unionReferences = (
  referenceGroups: SearchReference[][],
): SearchReference[] => {
  if (referenceGroups.length === 0) {
    return [];
  }

  if (referenceGroups.length === 1) {
    return referenceGroups[0];
  }

  const referenceMap = new Map<string, SearchReference>();

  for (const group of referenceGroups) {
    for (const reference of group) {
      const key = createReferenceKey(reference);
      const existing = referenceMap.get(key);
      if (existing === undefined || reference.score > existing.score) {
        referenceMap.set(key, reference);
      }
    }
  }

  return Array.from(referenceMap.values());
};

const intersectReferences = (
  referenceGroups: SearchReference[][],
): SearchReference[] => {
  if (referenceGroups.length === 0) {
    return [];
  }

  if (referenceGroups.length === 1) {
    return referenceGroups[0];
  }

  const firstGroup = referenceGroups[0];
  const referenceKeySet = new Set(firstGroup.map(createReferenceKey));

  for (let index = 1; index < referenceGroups.length; index++) {
    const currentGroup = referenceGroups[index];
    const currentKeys = new Set(currentGroup.map(createReferenceKey));

    for (const key of referenceKeySet) {
      if (!currentKeys.has(key)) {
        referenceKeySet.delete(key);
      }
    }
  }

  return firstGroup.filter((reference) =>
    referenceKeySet.has(createReferenceKey(reference)),
  );
};

const extractIdentifiersByType = (
  references: SearchReference[],
): {
  articleIdentifiers: ArticleIdentifier[];
  memoIdentifiers: MemoIdentifier[];
  seriesIdentifiers: SeriesIdentifier[];
} => {
  const articleIdentifiers: ArticleIdentifier[] = [];
  const memoIdentifiers: MemoIdentifier[] = [];
  const seriesIdentifiers: SeriesIdentifier[] = [];

  for (const reference of references) {
    switch (reference.identifier.type) {
      case ContentType.ARTICLE:
        articleIdentifiers.push(
          reference.identifier.content as ArticleIdentifier,
        );
        break;
      case ContentType.MEMO:
        memoIdentifiers.push(reference.identifier.content as MemoIdentifier);
        break;
      case ContentType.SERIES:
        seriesIdentifiers.push(
          reference.identifier.content as SeriesIdentifier,
        );
        break;
    }
  }

  return { articleIdentifiers, memoIdentifiers, seriesIdentifiers };
};

const sortResults = (
  results: SearchResult[],
  sortBy: Sort | null,
  order: Order | null,
): SearchResult[] => {
  if (sortBy === null || order === null) {
    return results;
  }

  const sorted = [...results];

  sorted.sort((first, second) => {
    const firstDate =
      sortBy === Sort.LATEST
        ? first.timeline.updatedAt
        : first.timeline.createdAt;
    const secondDate =
      sortBy === Sort.LATEST
        ? second.timeline.updatedAt
        : second.timeline.createdAt;

    if (order === Order.DESC) {
      return secondDate.getTime() - firstDate.getTime();
    }
    return firstDate.getTime() - secondDate.getTime();
  });

  return sorted;
};

export const createSearchByTokenWorkflow =
  (validate: ValidateCriteria) =>
  (logger: Logger) =>
  (ofTokenIdentifiers: OfTokenIdentifiers) =>
  (ofArticleIdentifiers: OfArticleIdentifiers) =>
  (ofMemoIdentifiers: OfMemoIdentifiers) =>
  (ofSeriesIdentifiers: OfSeriesIdentifiers): SearchByTokenWorkflow =>
  (command: SearchByTokensCommand) => {
    logger.info("SearchByTokenWorkflow started", {
      criteria: command.payload,
    });

    return validate(command.payload)
      .toAsync()
      .tap((criteria) => {
        logger.debug("Validation passed", { criteria });
      })
      .tapError((errors) => {
        logger.warn("Validation failed", { errors });
      })
      .andThen((criteria) => {
        const ngramTokenIdentifiers =
          criteria.freeWord !== null
            ? generateNgramTokenIdentifiers(criteria.freeWord)
            : [];

        const tagTokenIdentifiers =
          criteria.tags !== null && criteria.tags.length > 0
            ? generateTagTokenIdentifiers(criteria.tags)
            : [];

        if (
          ngramTokenIdentifiers.length === 0 &&
          tagTokenIdentifiers.length === 0
        ) {
          return ok<SearchReference[], SearchTokenError>([]).toAsync();
        }

        const allTokenIdentifiers = [
          ...ngramTokenIdentifiers,
          ...tagTokenIdentifiers,
        ];

        return ofTokenIdentifiers(allTokenIdentifiers, false)
          .mapError((error): SearchTokenError => error)
          .map((tokens) => {
            const ngramTokenSet = new Set(ngramTokenIdentifiers);
            const tagTokenSet = new Set(tagTokenIdentifiers);

            const ngramReferenceGroups: SearchReference[][] = [];
            const tagReferenceGroups: SearchReference[][] = [];

            for (const token of tokens) {
              if (ngramTokenSet.has(token.identifier)) {
                ngramReferenceGroups.push(token.references);
              }
              if (tagTokenSet.has(token.identifier)) {
                tagReferenceGroups.push(token.references);
              }
            }

            const ngramRefs =
              ngramReferenceGroups.length > 0
                ? unionReferences(ngramReferenceGroups)
                : [];

            const tagRefs =
              tagReferenceGroups.length > 0
                ? intersectReferences(tagReferenceGroups)
                : [];

            let result: SearchReference[];

            if (ngramRefs.length > 0 && tagRefs.length > 0) {
              result = intersectReferences([ngramRefs, tagRefs]);
            } else if (ngramRefs.length > 0) {
              result = ngramRefs;
            } else {
              result = tagRefs;
            }

            if (criteria.type !== null) {
              return result.filter(
                (reference) => reference.identifier.type === criteria.type,
              );
            }

            return result;
          });
      })
      .andThen((references) => {
        logger.debug("References retrieved", { count: references.length });

        const { articleIdentifiers, memoIdentifiers, seriesIdentifiers } =
          extractIdentifiersByType(references);

        return ofArticleIdentifiers(articleIdentifiers)
          .andThen((articles) =>
            ofMemoIdentifiers(memoIdentifiers).andThen((memos) =>
              ofSeriesIdentifiers(seriesIdentifiers).map((series) => [
                ...articles,
                ...memos,
                ...series,
              ]),
            ),
          )
          .mapError((error): SearchTokenError => error);
      })
      .andThen((results) => {
        return validate(command.payload)
          .toAsync()
          .map((criteria) =>
            sortResults(results, criteria.sortBy, criteria.order),
          );
      })
      .tap((results) => {
        logger.info("SearchByTokenWorkflow completed", {
          resultCount: results.length,
        });
      })
      .tapError((error) => {
        logger.error("SearchByTokenWorkflow failed", { error });
      });
  };
