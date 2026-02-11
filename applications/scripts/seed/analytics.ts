import { createDocument, ARTICLE_IDS, MEMO_IDS, TAG_IDS } from "./common";
import { ulid } from "ulid";

const now = new Date();

function daysAgo(days: number): Date {
  return new Date(now.getTime() - days * 24 * 60 * 60 * 1000);
}

function toJstDateKey(date: Date): string {
  const jst = new Date(date.getTime() + 9 * 60 * 60 * 1000);
  return jst.toISOString().slice(0, 10);
}

const VISITOR_SESSIONS = Array.from({ length: 8 }, () => crypto.randomUUID());

type PageViewEntry = {
  contentId: string;
  contentType: string;
  day: number;
  sessionIndex: number;
  referrer: string | null;
  deviceType: string;
};

const PAGE_VIEW_ENTRIES: PageViewEntry[] = [
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 0, sessionIndex: 0, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 0, sessionIndex: 1, referrer: "https://www.google.com/search?q=test", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 0, sessionIndex: 2, referrer: "https://twitter.com/home", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 0, sessionIndex: 3, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 1, sessionIndex: 4, referrer: "https://www.google.com/search?q=typescript", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 1, sessionIndex: 5, referrer: "https://github.com/lihs-ie/hut", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 1, sessionIndex: 1, referrer: "https://www.google.com/search?q=react", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 1, sessionIndex: 6, referrer: "https://www.google.com/search?q=nextjs", deviceType: "tablet" },
  { contentId: MEMO_IDS.memo1, contentType: "memo", day: 1, sessionIndex: 7, referrer: null, deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 2, sessionIndex: 0, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 2, sessionIndex: 2, referrer: "https://www.google.com/search?q=ts", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 2, sessionIndex: 3, referrer: null, deviceType: "desktop" },
  { contentId: MEMO_IDS.memo1, contentType: "memo", day: 2, sessionIndex: 4, referrer: "https://www.google.com/search?q=memo", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 3, sessionIndex: 5, referrer: "https://github.com/lihs-ie/hut", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 3, sessionIndex: 6, referrer: "https://twitter.com/home", deviceType: "tablet" },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 3, sessionIndex: 7, referrer: null, deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 4, sessionIndex: 0, referrer: "https://www.google.com/search?q=article", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 4, sessionIndex: 1, referrer: null, deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 4, sessionIndex: 2, referrer: "https://www.google.com/search?q=article2", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 4, sessionIndex: 3, referrer: null, deviceType: "desktop" },
  { contentId: MEMO_IDS.memo1, contentType: "memo", day: 4, sessionIndex: 4, referrer: "https://www.google.com/search?q=memo2", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 5, sessionIndex: 5, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 5, sessionIndex: 6, referrer: "https://www.google.com/search?q=nextjs2", deviceType: "tablet" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 6, sessionIndex: 7, referrer: "https://www.google.com/search?q=ts2", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 6, sessionIndex: 0, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 7, sessionIndex: 0, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 7, sessionIndex: 1, referrer: "https://www.google.com/search?q=react2", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 8, sessionIndex: 2, referrer: "https://www.google.com/search?q=ts3", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 9, sessionIndex: 3, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 9, sessionIndex: 4, referrer: null, deviceType: "mobile" },
  { contentId: MEMO_IDS.memo1, contentType: "memo", day: 9, sessionIndex: 5, referrer: "https://www.google.com/search?q=memo3", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 10, sessionIndex: 6, referrer: "https://www.google.com/search?q=ts4", deviceType: "tablet" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 10, sessionIndex: 7, referrer: "https://twitter.com/home", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 11, sessionIndex: 0, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 11, sessionIndex: 1, referrer: "https://www.google.com/search?q=ts5", deviceType: "mobile" },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 12, sessionIndex: 2, referrer: null, deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 12, sessionIndex: 3, referrer: "https://www.google.com/search?q=nextjs3", deviceType: "desktop" },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 13, sessionIndex: 4, referrer: null, deviceType: "mobile" },
];

async function seedPageViews(): Promise<void> {
  console.log("  Creating PageView data...");

  const counters = new Map<string, number>();

  for (const entry of PAGE_VIEW_ENTRIES) {
    const dateKey = toJstDateKey(daysAgo(entry.day));
    const sessionKey = VISITOR_SESSIONS[entry.sessionIndex];
    const compositeId = `${dateKey}:${sessionKey}`;
    const createdAt = daysAgo(entry.day);

    await createDocument(
      `access-logs/${entry.contentType}/${entry.contentId}`,
      compositeId,
      {
        referrer: entry.referrer,
        deviceType: entry.deviceType,
        sessionKey: sessionKey,
        createdAt: createdAt,
      },
      { useTimestamp: true },
    );

    await createDocument(
      `page-view-dedup/${entry.contentType}/${entry.contentId}`,
      compositeId,
      { createdAt: createdAt },
      { useTimestamp: true },
    );

    const counterKey = `${entry.contentType}/${entry.contentId}/${dateKey}`;
    counters.set(counterKey, (counters.get(counterKey) ?? 0) + 1);
  }

  for (const [key, count] of counters) {
    const parts = key.split("/");
    await createDocument(
      `page-view-counters/${parts[0]}/${parts[1]}`,
      parts[2],
      { count, updatedAt: now },
      { useTimestamp: true },
    );
  }

  const contentIdsByType = new Map<string, Set<string>>();
  for (const entry of PAGE_VIEW_ENTRIES) {
    const ids = contentIdsByType.get(entry.contentType) ?? new Set<string>();
    ids.add(entry.contentId);
    contentIdsByType.set(entry.contentType, ids);
  }

  for (const [contentType, contentIds] of contentIdsByType) {
    await createDocument("access-logs", contentType, {
      contentIds: Array.from(contentIds),
    });
  }
}

async function seedUniqueVisitors(): Promise<void> {
  console.log("  Creating UniqueVisitor data...");

  const dailyVisitors = new Map<string, Set<number>>();

  for (const entry of PAGE_VIEW_ENTRIES) {
    const dateKey = toJstDateKey(daysAgo(entry.day));
    const visitors = dailyVisitors.get(dateKey) ?? new Set<number>();
    visitors.add(entry.sessionIndex);
    dailyVisitors.set(dateKey, visitors);
  }

  for (const [dateKey, sessionIndices] of dailyVisitors) {
    for (const sessionIndex of sessionIndices) {
      const sessionKey = VISITOR_SESSIONS[sessionIndex];
      await createDocument(
        "unique-visitor-dedup",
        `${dateKey}:${sessionKey}`,
        { createdAt: daysAgo(0) },
        { useTimestamp: true },
      );
    }

    await createDocument(
      "unique-visitor-counters",
      dateKey,
      { count: sessionIndices.size, updatedAt: now },
      { useTimestamp: true },
    );
  }
}

type EngagementEntry = {
  contentId: string;
  contentType: string;
  day: number;
  sessionIndex: number;
  dwellTime: number;
  scrollDepth: number;
};

const ENGAGEMENT_ENTRIES: EngagementEntry[] = [
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 0, sessionIndex: 0, dwellTime: 180, scrollDepth: 85 },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 1, sessionIndex: 4, dwellTime: 240, scrollDepth: 90 },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 2, sessionIndex: 0, dwellTime: 120, scrollDepth: 60 },
  { contentId: ARTICLE_IDS.article1, contentType: "article", day: 4, sessionIndex: 0, dwellTime: 300, scrollDepth: 95 },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 0, sessionIndex: 2, dwellTime: 90, scrollDepth: 45 },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 1, sessionIndex: 1, dwellTime: 150, scrollDepth: 70 },
  { contentId: ARTICLE_IDS.article2, contentType: "article", day: 4, sessionIndex: 2, dwellTime: 60, scrollDepth: 30 },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 1, sessionIndex: 6, dwellTime: 200, scrollDepth: 80 },
  { contentId: ARTICLE_IDS.article4, contentType: "article", day: 4, sessionIndex: 3, dwellTime: 45, scrollDepth: 25 },
  { contentId: MEMO_IDS.memo1, contentType: "memo", day: 1, sessionIndex: 7, dwellTime: 100, scrollDepth: 100 },
  { contentId: MEMO_IDS.memo1, contentType: "memo", day: 2, sessionIndex: 4, dwellTime: 80, scrollDepth: 90 },
  { contentId: MEMO_IDS.memo1, contentType: "memo", day: 4, sessionIndex: 4, dwellTime: 70, scrollDepth: 85 },
];

async function seedEngagementRecords(): Promise<void> {
  console.log("  Creating EngagementRecord data...");

  const contentIdsByType = new Map<string, Set<string>>();

  for (const entry of ENGAGEMENT_ENTRIES) {
    const dateKey = toJstDateKey(daysAgo(entry.day));
    const sessionKey = VISITOR_SESSIONS[entry.sessionIndex];
    const compositeId = `${dateKey}:${sessionKey}`;
    const createdAt = daysAgo(entry.day);

    await createDocument(
      `engagement-logs/${entry.contentType}/${entry.contentId}`,
      compositeId,
      {
        dwellTime: entry.dwellTime,
        maxScrollDepth: entry.scrollDepth,
        createdAt,
        updatedAt: createdAt,
      },
      { useTimestamp: true },
    );

    const ids = contentIdsByType.get(entry.contentType) ?? new Set<string>();
    ids.add(entry.contentId);
    contentIdsByType.set(entry.contentType, ids);
  }

  for (const [contentType, contentIds] of contentIdsByType) {
    await createDocument("engagement-logs", contentType, {
      contentIds: Array.from(contentIds),
    });
  }
}

type SearchEntry = {
  day: number;
  keyword: string;
  resultCount: number;
  tags: string[] | null;
  contentType: string | null;
};

const SEARCH_ENTRIES: SearchEntry[] = [
  { day: 0, keyword: "TypeScript", resultCount: 3, tags: [TAG_IDS.typescript], contentType: "article" },
  { day: 0, keyword: "React hooks", resultCount: 2, tags: [TAG_IDS.react], contentType: "article" },
  { day: 1, keyword: "Next.js", resultCount: 1, tags: [TAG_IDS.nextjs], contentType: "article" },
  { day: 1, keyword: "Go言語", resultCount: 1, tags: [TAG_IDS.go], contentType: "memo" },
  { day: 1, keyword: "GraphQL", resultCount: 0, tags: null, contentType: null },
  { day: 2, keyword: "TypeScript", resultCount: 3, tags: [TAG_IDS.typescript], contentType: "article" },
  { day: 2, keyword: "コンポーネント設計", resultCount: 2, tags: [TAG_IDS.react], contentType: "article" },
  { day: 3, keyword: "React", resultCount: 2, tags: [TAG_IDS.react], contentType: "article" },
  { day: 3, keyword: "Rust入門", resultCount: 0, tags: null, contentType: null },
  { day: 4, keyword: "TypeScript", resultCount: 3, tags: [TAG_IDS.typescript], contentType: "article" },
  { day: 4, keyword: "型安全", resultCount: 1, tags: [TAG_IDS.typescript], contentType: "article" },
  { day: 4, keyword: "Docker", resultCount: 0, tags: null, contentType: null },
  { day: 5, keyword: "Next.js", resultCount: 1, tags: [TAG_IDS.nextjs], contentType: "article" },
  { day: 6, keyword: "TypeScript", resultCount: 3, tags: [TAG_IDS.typescript], contentType: "article" },
  { day: 6, keyword: "Go", resultCount: 1, tags: [TAG_IDS.go], contentType: "memo" },
];

async function seedSearchRecords(): Promise<void> {
  console.log("  Creating SearchRecord data...");

  for (const entry of SEARCH_ENTRIES) {
    const dateKey = toJstDateKey(daysAgo(entry.day));
    const recordId = ulid();
    const createdAt = daysAgo(entry.day);

    await createDocument(
      `search-logs/${dateKey}/records`,
      recordId,
      {
        keyword: entry.keyword,
        resultCount: entry.resultCount,
        tags: entry.tags,
        type: entry.contentType,
        createdAt,
      },
      { useTimestamp: true },
    );
  }
}

export async function seedAnalytics(): Promise<void> {
  console.log("\n--- Creating Analytics Data ---");
  await seedPageViews();
  await seedUniqueVisitors();
  await seedEngagementRecords();
  await seedSearchRecords();
}
