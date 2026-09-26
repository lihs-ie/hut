import { unstable_cache } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article } from "@shared/domains/articles";
import {
  restoreDateFromCache,
  restoreTimelineFromCache,
} from "@shared/aspects/cache";
import { ContentType, UnvalidatedCriteria, validateCriteria } from "@shared/domains/search-token";
import { ArticleWorkflowProvider } from "@/providers/workflows/article";
import { PublishStatus } from "@shared/domains/common";

type CachedSearchResult = {
  identifier: string;
  publishedAt?: string | Date | null;
  timeline?: { createdAt: string | Date; updatedAt: string | Date };
};

const restorePublishedAt = (
  value: string | Date | null | undefined,
): Date | null => {
  if (value == null) return null;
  return restoreDateFromCache(value);
};

const restoreSearchResultDates = (
  results: CachedSearchResult[],
): Article[] => {
  return results.map((item) => {
    const timeline = item.timeline
      ? restoreTimelineFromCache(item.timeline)
      : undefined;
    const publishedAt = restorePublishedAt(item.publishedAt);

    return {
      ...item,
      timeline,
      publishedAt,
    } as Article;
  });
};

const searchByTokenInternal = async (
  unvalidated: UnvalidatedCriteria,
): Promise<Article[]> => {
  const criteria = await unwrapForNextJs(validateCriteria(unvalidated).toAsync());
  if (criteria.type !== null && criteria.type !== ContentType.ARTICLE) {
    return [];
  }
  if (criteria.freeWord === null && !criteria.tags?.length
    && criteria.type === null) {
    return [];
  }
  const articles = await unwrapForNextJs(
    ArticleWorkflowProvider.search({
      payload: {
        freeWord: criteria.freeWord,
        tags: criteria.tags,
        status: PublishStatus.PUBLISHED,
      },
      now: new Date(),
    }),
  );
  if (criteria.sortBy === null || criteria.order === null) return articles;
  return articles.sort((left, right) => {
    const leftDate = criteria.sortBy === "latest"
      ? left.timeline.updatedAt
      : left.timeline.createdAt;
    const rightDate = criteria.sortBy === "latest"
      ? right.timeline.updatedAt
      : right.timeline.createdAt;
    const direction = criteria.order === "desc" ? -1 : 1;
    return direction * (leftDate.getTime() - rightDate.getTime());
  });
};

export const searchByToken = async (
  unvalidated: UnvalidatedCriteria,
): Promise<Article[]> => {
  const cachedResults = await unstable_cache(
    () => searchByTokenInternal(unvalidated),
    ["search-token", JSON.stringify(unvalidated)],
    { revalidate: 3600, tags: ["search-token"] },
  )();

  return restoreSearchResultDates(cachedResults as CachedSearchResult[]);
};
