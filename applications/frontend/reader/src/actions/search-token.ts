import { unstable_cache } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article } from "@shared/domains/articles";
import {
  restoreDateFromCache,
  restoreTimelineFromCache,
} from "@shared/aspects/cache";
import { Memo, MemoEntry } from "@shared/domains/memo";
import { UnvalidatedCriteria } from "@shared/domains/search-token";
import { Series } from "@shared/domains/series";
import { ReaderSearchTokenWorkflowProvider } from "@/providers/workflows/search-token";

type CachedSearchResult = {
  identifier: string;
  publishedAt?: string | Date | null;
  timeline?: { createdAt: string | Date; updatedAt: string | Date };
  content?: string;
  excerpt?: string;
  chapters?: string[];
  subTitle?: string | null;
  cover?: string | null;
  entries?: Array<{ text: string; createdAt: string | Date }>;
};

const restorePublishedAt = (
  value: string | Date | null | undefined,
): Date | null => {
  if (value == null) return null;
  return restoreDateFromCache(value);
};

const restoreSearchResultDates = (
  results: CachedSearchResult[],
): (Article | Series | Memo)[] => {
  return results.map((item) => {
    const timeline = item.timeline
      ? restoreTimelineFromCache(item.timeline)
      : undefined;
    const publishedAt = restorePublishedAt(item.publishedAt);

    if ("entries" in item && Array.isArray(item.entries)) {
      const entries = item.entries.map(
        (entry) =>
          ({
            text: entry.text,
            createdAt: restoreDateFromCache(entry.createdAt),
          }) as MemoEntry,
      );
      return {
        ...item,
        timeline,
        publishedAt,
        entries,
      } as Memo;
    }

    if ("chapters" in item && Array.isArray(item.chapters)) {
      return {
        ...item,
        timeline,
        publishedAt,
        chapters: item.chapters,
      } as Series;
    }

    return {
      ...item,
      timeline,
      publishedAt,
    } as Article;
  });
};

const searchByTokenInternal = async (
  unvalidated: UnvalidatedCriteria,
): Promise<(Article | Series | Memo)[]> => {
  return await unwrapForNextJs(
    ReaderSearchTokenWorkflowProvider.search({
      payload: unvalidated,
      now: new Date(),
    }),
  );
};

export const searchByToken = async (
  unvalidated: UnvalidatedCriteria,
): Promise<(Article | Series | Memo)[]> => {
  const cachedResults = await unstable_cache(
    () => searchByTokenInternal(unvalidated),
    ["search-token", JSON.stringify(unvalidated)],
    { revalidate: 3600, tags: ["search-token"] },
  )();

  return restoreSearchResultDates(cachedResults as CachedSearchResult[]);
};
