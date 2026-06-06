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
import { SearchTokenWorkflowProvider } from "@shared/providers/workflows/search-token";
import { recordSearchLog } from "@shared/actions/search-log";

/**
 * キャッシュから取得した検索結果のDateフィールドを復元する。
 *
 * unstable_cacheはJSON.stringify/parseを使うため、
 * DateオブジェクトがISO文字列になってしまう。
 * この関数でArticle, Series, MemoのDateフィールドを復元する。
 */
type CachedSearchResult = {
  identifier: string;
  publishedAt?: string | Date | null;
  timeline?: { createdAt: string | Date; updatedAt: string | Date };
  // Article specific
  content?: string;
  excerpt?: string;
  // Series specific
  chapters?: string[];
  subTitle?: string | null;
  cover?: string | null;
  // Memo specific
  entries?: Array<{ text: string; createdAt: string | Date }>;
};

const restorePublishedAt = (
  value: string | Date | null | undefined,
): Date | null => {
  if (value == null) return null;
  return restoreDateFromCache(value);
};

const restoreSearchResultDates = (
  results: CachedSearchResult[]
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
          }) as MemoEntry
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
  unvalidated: UnvalidatedCriteria
): Promise<(Article | Series | Memo)[]> => {
  return await unwrapForNextJs(
    SearchTokenWorkflowProvider.search({
      payload: unvalidated,
      now: new Date(),
    })
  );
};

export const searchByToken = async (
  unvalidated: UnvalidatedCriteria
): Promise<(Article | Series | Memo)[]> => {
  const cachedResults = await unstable_cache(
    () => searchByTokenInternal(unvalidated),
    ["search-token", JSON.stringify(unvalidated)],
    { revalidate: 3600, tags: ["search-token"] }
  )();

  const results = restoreSearchResultDates(
    cachedResults as CachedSearchResult[],
  );

  // 検索ログを非同期で記録（検索結果の返却をブロックしない）
  void recordSearchLog(unvalidated, results.length);

  return results;
};
