import type { Timeline } from "@shared/domains/common/date";

/**
 * キャッシュから取得したデータのDateフィールドを復元するヘルパー関数。
 *
 * unstable_cacheはJSON.stringify/parseを使うため、
 * DateオブジェクトがISO文字列になる。この関数で復元する。
 *
 * @param value - 復元する値（Date | string）
 * @returns Date オブジェクト
 */
export const restoreDateFromCache = (value: Date | string): Date => {
  if (value instanceof Date) {
    return value;
  }
  return new Date(value);
};

/**
 * キャッシュから取得したTimelineを復元するヘルパー関数。
 * キャッシュからのデータでは、createdAt/updatedAtがDateまたは文字列のいずれかになる可能性がある。
 */
export const restoreTimelineFromCache = (timeline: {
  createdAt: Date | string;
  updatedAt: Date | string;
}): Timeline => {
  return {
    createdAt: restoreDateFromCache(timeline.createdAt),
    updatedAt: restoreDateFromCache(timeline.updatedAt),
  };
};
