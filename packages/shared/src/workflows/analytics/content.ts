import { UnexpectedError, ValidationError } from "@shared/aspects/error";
import { AsyncResult, Result, combineAsync } from "@shared/aspects/result";
import { Logger } from "@shared/aspects/logger";
import { Command } from "@shared/workflows/common";
import {
  PageView,
  Criteria as PageViewCriteria,
  criteriaSchema as pageViewCriteriaSchema,
} from "@shared/domains/analytics/page-view";
import {
  DateRange,
  Period,
  RankedItem,
  Distribution,
  resolveDateRange,
  validateRankedItem,
} from "@shared/domains/analytics/common";
import { Article } from "@shared/domains/articles";
import { TagIdentifier } from "@shared/domains/attributes/tag";
import { toSortedRankedItems, toSortedDistributions } from "./page-view";
import {
  type SearchArticles,
  type SearchMemos,
  buildEmptyArticleCriteria,
  buildEmptyMemoCriteria,
  buildContentTitleMap,
  resolveRankedItemTitles,
} from "./title-resolution";


type ValidatePeriod = (
  period: string
) => Result<Period, ValidationError>;

type SearchPageViews = (
  criteria: PageViewCriteria
) => AsyncResult<PageView[], UnexpectedError>;

const buildPageViewSearchCriteria = (dateRange: DateRange): PageViewCriteria =>
  pageViewCriteriaSchema.parse({ dateRange });

export const aggregateByContent = (pageViews: PageView[]): RankedItem[] => {
  const contentTotals = new Map<string, { total: number; type: string }>();

  for (const pageView of pageViews) {
    const key = pageView.identifier.reference.content;
    const existing = contentTotals.get(key);

    if (existing) {
      existing.total += 1;
    } else {
      contentTotals.set(key, {
        total: 1,
        type: pageView.identifier.reference.type,
      });
    }
  }

  const items: RankedItem[] = [];

  for (const [contentIdentifier, data] of contentTotals) {
    items.push(
      validateRankedItem({
        label: contentIdentifier,
        value: data.total,
        subLabel: data.type,
      }).unwrap()
    );
  }

  return items.toSorted((first, second) => second.value - first.value);
};

export const buildArticleTagMap = (
  articles: Article[]
): Map<string, TagIdentifier[]> => {
  const tagMap = new Map<string, TagIdentifier[]>();

  for (const article of articles) {
    tagMap.set(article.identifier, [...article.tags]);
  }

  return tagMap;
};

export const aggregateByTag = (
  pageViews: PageView[],
  articleTagMap: Map<string, TagIdentifier[]>
): RankedItem[] => {
  const contentTotals = new Map<string, number>();

  for (const pageView of pageViews) {
    const key = pageView.identifier.reference.content;
    contentTotals.set(key, (contentTotals.get(key) ?? 0) + 1);
  }

  const tagTotals = new Map<TagIdentifier, number>();

  for (const [contentIdentifier, totalPageViews] of contentTotals) {
    const tags = articleTagMap.get(contentIdentifier) ?? [];

    for (const tag of tags) {
      tagTotals.set(tag, (tagTotals.get(tag) ?? 0) + totalPageViews);
    }
  }

  return toSortedRankedItems(tagTotals);
};

export const aggregateByContentType = (pageViews: PageView[]): Distribution[] => {
  const typeTotals = new Map<string, number>();

  for (const pageView of pageViews) {
    const contentType = pageView.identifier.reference.type;
    typeTotals.set(contentType, (typeTotals.get(contentType) ?? 0) + 1);
  }

  return toSortedDistributions(typeTotals);
};

export type GetContentRankingWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<RankedItem[], ValidationError | UnexpectedError>;

export type GetTagPageViewsWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<RankedItem[], ValidationError | UnexpectedError>;

export type GetContentTypeComparisonWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<Distribution[], ValidationError | UnexpectedError>;

export const createGetContentRankingWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchPageViews) =>
  (searchArticles: SearchArticles) =>
  (searchMemos: SearchMemos) =>
  (logger: Logger): GetContentRankingWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetContentRankingWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .andThen((period) => {
        const dateRange = resolveDateRange(period);
        return combineAsync([
          search(buildPageViewSearchCriteria(dateRange)),
          searchArticles(buildEmptyArticleCriteria()),
          searchMemos(buildEmptyMemoCriteria()),
        ] as const);
      })
      .map(([pageViews, articles, memos]) => {
        const ranking = aggregateByContent(pageViews);
        const titleMap = buildContentTitleMap(articles, memos);
        return resolveRankedItemTitles(ranking, titleMap);
      })
      .tap((ranking) => {
        logger.info("GetContentRankingWorkflow completed", {
          rankingSize: ranking.length,
        });
      })
      .tapError((error) => {
        logger.error("GetContentRankingWorkflow failed", { error });
      });
  };

export const createGetTagPageViewsWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (searchPageViews: SearchPageViews) =>
  (searchArticles: SearchArticles) =>
  (logger: Logger): GetTagPageViewsWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetTagPageViewsWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .andThen((period) => {
        const dateRange = resolveDateRange(period);
        return searchPageViews(buildPageViewSearchCriteria(dateRange)).andThen(
          (pageViews) =>
            searchArticles(buildEmptyArticleCriteria()).map((articles) =>
              aggregateByTag(pageViews, buildArticleTagMap(articles))
            )
        );
      })
      .tap((ranking) => {
        logger.info("GetTagPageViewsWorkflow completed", {
          rankingSize: ranking.length,
        });
      })
      .tapError((error) => {
        logger.error("GetTagPageViewsWorkflow failed", { error });
      });
  };

export const createGetContentTypeComparisonWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchPageViews) =>
  (logger: Logger): GetContentTypeComparisonWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetContentTypeComparisonWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .andThen((period) => {
        const dateRange = resolveDateRange(period);
        return search(buildPageViewSearchCriteria(dateRange));
      })
      .map((pageViews) => aggregateByContentType(pageViews))
      .tap((distribution) => {
        logger.info("GetContentTypeComparisonWorkflow completed", {
          distributionCount: distribution.length,
        });
      })
      .tapError((error) => {
        logger.error("GetContentTypeComparisonWorkflow failed", { error });
      });
  };
