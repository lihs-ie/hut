export const REVALIDATION_TAGS = {
  ARTICLES: "articles",
  MEMOS: "memos",
  SERIES: "series",
  CHAPTERS: "chapters",
  TAGS: "tags",
  PRIVACY_POLICY: "privacy-policy",
} as const;

export type RevalidationTag =
  (typeof REVALIDATION_TAGS)[keyof typeof REVALIDATION_TAGS];

export const memoEntriesTag = (slug: string): string =>
  `memo-entries-${slug}`;
