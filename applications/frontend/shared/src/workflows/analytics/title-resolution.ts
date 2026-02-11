import { UnexpectedError } from "@shared/aspects/error";
import { AsyncResult } from "@shared/aspects/result";
import {
  RankedItem,
  validateRankedItem,
} from "@shared/domains/analytics/common";
import {
  Article,
  Criteria as ArticleCriteria,
  criteriaSchema as articleCriteriaSchema,
} from "@shared/domains/articles";
import {
  Memo,
  Criteria as MemoCriteria,
  criteriaSchema as memoCriteriaSchema,
} from "@shared/domains/memo";

export type SearchArticles = (
  criteria: ArticleCriteria
) => AsyncResult<Article[], UnexpectedError>;

export type SearchMemos = (
  criteria: MemoCriteria
) => AsyncResult<Memo[], UnexpectedError>;

export const buildEmptyArticleCriteria = (): ArticleCriteria =>
  articleCriteriaSchema.parse({});

export const buildEmptyMemoCriteria = (): MemoCriteria =>
  memoCriteriaSchema.parse({ tags: null, freeWord: null, status: null });

export const buildContentTitleMap = (
  articles: Article[],
  memos: Memo[]
): Map<string, string> => {
  const titleMap = new Map<string, string>();

  for (const article of articles) {
    titleMap.set(article.identifier, article.title);
  }

  for (const memo of memos) {
    titleMap.set(memo.identifier, memo.title);
  }

  return titleMap;
};

export const resolveRankedItemTitles = (
  items: RankedItem[],
  titleMap: Map<string, string>
): RankedItem[] =>
  items.map((item) => {
    const resolvedTitle = titleMap.get(item.label);
    if (resolvedTitle) {
      return validateRankedItem({
        label: resolvedTitle,
        value: item.value,
        subLabel: item.subLabel,
      }).unwrap();
    }
    return item;
  });
