import { describe, it, expect, vi, beforeEach } from "vitest";
import { pageViewSchema } from "@shared/domains/analytics/page-view";
import { engagementRecordSchema } from "@shared/domains/analytics/engagement";
import { uniqueVisitorSchema } from "@shared/domains/analytics/unique-visitor";
import { searchRecordSchema } from "@shared/domains/analytics/search-record";
import { articleSchema } from "@shared/domains/articles";
import { memoSchema } from "@shared/domains/memo";

const ULID_A1 = "01JMABCDEF0123456789ABCDE1";
const ULID_A2 = "01JMABCDEF0123456789ABCDE2";
const ULID_A3 = "01JMABCDEF0123456789ABCDE3";
const ULID_A4 = "01JMABCDEF0123456789ABCDE4";
const ULID_M1 = "01JMABCDEF0123456789ABCDE5";
const ULID_TAG1 = "01JMABCDEF0123456789ABCDE6";
const ULID_TAG2 = "01JMABCDEF0123456789ABCDE7";

const mockLoadCurrentPageViews = vi.fn();
const mockLoadPreviousPageViews = vi.fn();
const mockLoadCurrentEngagement = vi.fn();
const mockLoadPreviousEngagement = vi.fn();
const mockLoadCurrentUniqueVisitors = vi.fn();
const mockLoadPreviousUniqueVisitors = vi.fn();
const mockLoadCurrentSearchRecords = vi.fn();
const mockLoadPreviousSearchRecords = vi.fn();
const mockLoadZeroHitSearchRecords = vi.fn();
const mockLoadAllArticles = vi.fn();
const mockLoadAllMemos = vi.fn();

vi.mock("@/actions/analytics-data-loader", () => ({
  loadCurrentPageViews: (...args: unknown[]) =>
    mockLoadCurrentPageViews(...args),
  loadPreviousPageViews: (...args: unknown[]) =>
    mockLoadPreviousPageViews(...args),
  loadCurrentEngagement: (...args: unknown[]) =>
    mockLoadCurrentEngagement(...args),
  loadPreviousEngagement: (...args: unknown[]) =>
    mockLoadPreviousEngagement(...args),
  loadCurrentUniqueVisitors: (...args: unknown[]) =>
    mockLoadCurrentUniqueVisitors(...args),
  loadPreviousUniqueVisitors: (...args: unknown[]) =>
    mockLoadPreviousUniqueVisitors(...args),
  loadCurrentSearchRecords: (...args: unknown[]) =>
    mockLoadCurrentSearchRecords(...args),
  loadPreviousSearchRecords: (...args: unknown[]) =>
    mockLoadPreviousSearchRecords(...args),
  loadZeroHitSearchRecords: (...args: unknown[]) =>
    mockLoadZeroHitSearchRecords(...args),
  loadAllArticles: (...args: unknown[]) => mockLoadAllArticles(...args),
  loadAllMemos: (...args: unknown[]) => mockLoadAllMemos(...args),
}));

vi.mock("next/navigation", () => ({
  notFound: () => {
    throw new Error("NOT_FOUND");
  },
}));

function buildPageView(
  contentType: string,
  contentId: string,
  dateKey: string,
  deviceType: string,
  referrerRaw: string | null,
) {
  return pageViewSchema.parse({
    identifier: {
      reference: { type: contentType, content: contentId },
      dateKey,
      sessionKey: crypto.randomUUID(),
    },
    referrer: { raw: referrerRaw },
    deviceType,
    createdAt: new Date(),
  });
}

function buildEngagementRecord(
  contentType: string,
  contentId: string,
  dateKey: string,
  dwellTime: number,
  scrollDepth: number,
) {
  return engagementRecordSchema.parse({
    identifier: {
      reference: { type: contentType, content: contentId },
      dateKey,
      sessionKey: crypto.randomUUID(),
    },
    dwellTime,
    scrollDepth,
    createdAt: new Date(),
    updatedAt: new Date(),
  });
}

function buildUniqueVisitor(dateKey: string) {
  return uniqueVisitorSchema.parse({
    identifier: {
      dateKey,
      sessionKey: crypto.randomUUID(),
    },
    createdAt: new Date(),
  });
}

function buildSearchRecord(
  keyword: string,
  dateKey: string,
  resultCount: number,
) {
  return searchRecordSchema.parse({
    identifier: "01JMABCDEF0123456789ABCDEF",
    dateKey,
    keyword,
    resultCount,
    tags: null,
    contentType: null,
    createdAt: new Date(),
  });
}

function buildArticle(identifier: string, title: string, tags: string[]) {
  return articleSchema.parse({
    identifier,
    title,
    content: "content",
    excerpt: "excerpt",
    slug: "test-slug-" + identifier.slice(-6).toLowerCase(),
    status: "published",
    tags,
    timeline: {
      createdAt: new Date(),
      updatedAt: new Date(),
      publishedAt: new Date(),
    },
  });
}

function buildMemo(identifier: string, title: string) {
  return memoSchema.parse({
    identifier,
    title,
    slug: "test-slug-" + identifier.slice(-6).toLowerCase(),
    entries: [{ text: "entry text", createdAt: new Date() }],
    status: "published",
    tags: [],
    timeline: {
      createdAt: new Date(),
      updatedAt: new Date(),
      publishedAt: new Date(),
    },
  });
}


describe("analytics server actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("getTotalPageViews", () => {
    it("current/previous のページビュー数を PeriodComparison で返す", async () => {
      const currentPageViews = [
        buildPageView("article", ULID_A1, "2024-01-15", "desktop", null),
        buildPageView("article", ULID_A2, "2024-01-16", "mobile", null),
        buildPageView("memo", ULID_M1, "2024-01-15", "desktop", null),
      ];
      const previousPageViews = [
        buildPageView("article", ULID_A1, "2024-01-08", "desktop", null),
        buildPageView("article", ULID_A2, "2024-01-09", "mobile", null),
      ];
      mockLoadCurrentPageViews.mockResolvedValue(currentPageViews);
      mockLoadPreviousPageViews.mockResolvedValue(previousPageViews);

      const { getTotalPageViews } = await import("@/actions/analytics");
      const result = await getTotalPageViews("30d");

      expect(result.current).toBe(3);
      expect(result.previous).toBe(2);
    });

    it("ローダーが例外をスローした場合は例外が伝播する", async () => {
      mockLoadCurrentPageViews.mockRejectedValue(new Error("test error"));
      mockLoadPreviousPageViews.mockResolvedValue([]);

      const { getTotalPageViews } = await import("@/actions/analytics");
      await expect(getTotalPageViews("30d")).rejects.toThrow();
    });
  });

  describe("getPageViewTrend", () => {
    it("日付ごとに集計された TrendPoint 配列を返す", async () => {
      const pageViews = [
        buildPageView("article", ULID_A1, "2024-01-15", "desktop", null),
        buildPageView("article", ULID_A2, "2024-01-15", "mobile", null),
        buildPageView("article", ULID_A3, "2024-01-16", "desktop", null),
      ];
      mockLoadCurrentPageViews.mockResolvedValue(pageViews);

      const { getPageViewTrend } = await import("@/actions/analytics");
      const result = await getPageViewTrend("7d");

      expect(result).toHaveLength(2);
      expect(result[0].dateKey).toBe("2024-01-15");
      expect(result[0].value).toBe(2);
      expect(result[1].dateKey).toBe("2024-01-16");
      expect(result[1].value).toBe(1);
    });
  });

  describe("getReferrerRanking", () => {
    it("リファラードメインごとに集計された RankedItem 配列を返す", async () => {
      const pageViews = [
        buildPageView("article", ULID_A1, "2024-01-15", "desktop", "https://google.com/search"),
        buildPageView("article", ULID_A2, "2024-01-15", "desktop", "https://google.com/search"),
        buildPageView("article", ULID_A3, "2024-01-15", "desktop", null),
      ];
      mockLoadCurrentPageViews.mockResolvedValue(pageViews);

      const { getReferrerRanking } = await import("@/actions/analytics");
      const result = await getReferrerRanking("30d");

      expect(result).toHaveLength(2);
      expect(result[0].label).toBe("google.com");
      expect(result[0].value).toBe(2);
      expect(result[1].label).toBe("Direct");
      expect(result[1].value).toBe(1);
    });
  });

  describe("getDeviceDistribution", () => {
    it("デバイスタイプごとに集計された Distribution 配列を返す", async () => {
      const pageViews = [
        buildPageView("article", ULID_A1, "2024-01-15", "desktop", null),
        buildPageView("article", ULID_A2, "2024-01-15", "desktop", null),
        buildPageView("article", ULID_A3, "2024-01-15", "mobile", null),
      ];
      mockLoadCurrentPageViews.mockResolvedValue(pageViews);

      const { getDeviceDistribution } = await import("@/actions/analytics");
      const result = await getDeviceDistribution("30d");

      expect(result).toHaveLength(2);
      expect(result[0].label).toBe("desktop");
      expect(result[0].value).toBe(2);
    });
  });

  describe("getUniqueVisitors", () => {
    it("current/previous のユニークビジター数を PeriodComparison で返す", async () => {
      const current = [
        buildUniqueVisitor("2024-01-15"),
        buildUniqueVisitor("2024-01-16"),
        buildUniqueVisitor("2024-01-17"),
      ];
      const previous = [
        buildUniqueVisitor("2024-01-08"),
        buildUniqueVisitor("2024-01-09"),
      ];
      mockLoadCurrentUniqueVisitors.mockResolvedValue(current);
      mockLoadPreviousUniqueVisitors.mockResolvedValue(previous);

      const { getUniqueVisitors } = await import("@/actions/analytics");
      const result = await getUniqueVisitors("30d");

      expect(result.current).toBe(3);
      expect(result.previous).toBe(2);
    });
  });

  describe("getAverageDwellTime", () => {
    it("current/previous の平均滞在時間を PeriodComparison で返す", async () => {
      const current = [
        buildEngagementRecord("article", ULID_A1, "2024-01-15", 60, 50),
        buildEngagementRecord("article", ULID_A2, "2024-01-15", 120, 80),
      ];
      const previous = [
        buildEngagementRecord("article", ULID_A1, "2024-01-08", 40, 30),
        buildEngagementRecord("article", ULID_A2, "2024-01-08", 80, 60),
      ];
      mockLoadCurrentEngagement.mockResolvedValue(current);
      mockLoadPreviousEngagement.mockResolvedValue(previous);

      const { getAverageDwellTime } = await import("@/actions/analytics");
      const result = await getAverageDwellTime("7d");

      expect(result.current).toBe(90);
      expect(result.previous).toBe(60);
    });
  });

  describe("getDwellTimeRanking", () => {
    it("コンテンツごとの平均滞在時間をタイトル解決して返す", async () => {
      const records = [
        buildEngagementRecord("article", ULID_A1, "2024-01-15", 300, 80),
        buildEngagementRecord("memo", ULID_M1, "2024-01-15", 60, 50),
      ];
      const articles = [
        buildArticle(ULID_A1, "Popular Article", []),
      ];
      const memos = [
        buildMemo(ULID_M1, "Some Memo"),
      ];
      mockLoadCurrentEngagement.mockResolvedValue(records);
      mockLoadAllArticles.mockResolvedValue(articles);
      mockLoadAllMemos.mockResolvedValue(memos);

      const { getDwellTimeRanking } = await import("@/actions/analytics");
      const result = await getDwellTimeRanking("30d");

      expect(result[0].label).toBe("Popular Article");
      expect(result[0].value).toBe(300);
      expect(result[1].label).toBe("Some Memo");
    });
  });

  describe("getScrollDepthDistribution", () => {
    it("スクロール深度のバケットごとに集計された Distribution 配列を返す", async () => {
      const records = [
        buildEngagementRecord("article", ULID_A1, "2024-01-15", 60, 10),
        buildEngagementRecord("article", ULID_A2, "2024-01-15", 60, 40),
        buildEngagementRecord("article", ULID_A3, "2024-01-15", 60, 60),
        buildEngagementRecord("article", ULID_A4, "2024-01-15", 60, 90),
      ];
      mockLoadCurrentEngagement.mockResolvedValue(records);

      const { getScrollDepthDistribution } = await import(
        "@/actions/analytics"
      );
      const result = await getScrollDepthDistribution("30d");

      expect(result).toHaveLength(4);
      expect(result[0].label).toBe("0-25%");
      expect(result[0].value).toBe(1);
    });
  });

  describe("getSearchCount", () => {
    it("current/previous の検索数を PeriodComparison で返す", async () => {
      const current = [
        buildSearchRecord("react", "2024-01-15", 5),
        buildSearchRecord("next", "2024-01-15", 3),
      ];
      const previous = [
        buildSearchRecord("react", "2024-01-08", 2),
      ];
      mockLoadCurrentSearchRecords.mockResolvedValue(current);
      mockLoadPreviousSearchRecords.mockResolvedValue(previous);

      const { getSearchCount } = await import("@/actions/analytics");
      const result = await getSearchCount("30d");

      expect(result.current).toBe(2);
      expect(result.previous).toBe(1);
    });
  });

  describe("getSearchKeywordRanking", () => {
    it("キーワードごとに集計された RankedItem 配列を返す", async () => {
      const records = [
        buildSearchRecord("react", "2024-01-15", 5),
        buildSearchRecord("react", "2024-01-16", 3),
        buildSearchRecord("nextjs", "2024-01-15", 2),
      ];
      mockLoadCurrentSearchRecords.mockResolvedValue(records);

      const { getSearchKeywordRanking } = await import("@/actions/analytics");
      const result = await getSearchKeywordRanking("30d");

      expect(result[0].label).toBe("react");
      expect(result[0].value).toBe(2);
    });
  });

  describe("getSearchCountTrend", () => {
    it("日付ごとに集計された TrendPoint 配列を返す", async () => {
      const records = [
        buildSearchRecord("react", "2024-01-15", 5),
        buildSearchRecord("next", "2024-01-15", 3),
        buildSearchRecord("react", "2024-01-16", 2),
      ];
      mockLoadCurrentSearchRecords.mockResolvedValue(records);

      const { getSearchCountTrend } = await import("@/actions/analytics");
      const result = await getSearchCountTrend("30d");

      expect(result[0].dateKey).toBe("2024-01-15");
      expect(result[0].value).toBe(2);
      expect(result[1].dateKey).toBe("2024-01-16");
      expect(result[1].value).toBe(1);
    });
  });

  describe("getZeroHitKeywords", () => {
    it("ゼロヒットキーワードを集計して RankedItem 配列を返す", async () => {
      const records = [
        buildSearchRecord("nonexistent", "2024-01-15", 0),
        buildSearchRecord("nonexistent", "2024-01-16", 0),
        buildSearchRecord("missing", "2024-01-15", 0),
      ];
      mockLoadZeroHitSearchRecords.mockResolvedValue(records);

      const { getZeroHitKeywords } = await import("@/actions/analytics");
      const result = await getZeroHitKeywords("30d");

      expect(result[0].label).toBe("nonexistent");
      expect(result[0].value).toBe(2);
    });
  });

  describe("getContentRanking", () => {
    it("コンテンツごとのPV数をタイトル解決して返す", async () => {
      const pageViews = [
        buildPageView("article", ULID_A1, "2024-01-15", "desktop", null),
        buildPageView("article", ULID_A1, "2024-01-16", "desktop", null),
        buildPageView("memo", ULID_M1, "2024-01-15", "desktop", null),
      ];
      const articles = [
        buildArticle(ULID_A1, "Popular Article", []),
      ];
      const memos = [
        buildMemo(ULID_M1, "Normal Memo"),
      ];
      mockLoadCurrentPageViews.mockResolvedValue(pageViews);
      mockLoadAllArticles.mockResolvedValue(articles);
      mockLoadAllMemos.mockResolvedValue(memos);

      const { getContentRanking } = await import("@/actions/analytics");
      const result = await getContentRanking("30d");

      expect(result[0].label).toBe("Popular Article");
      expect(result[0].value).toBe(2);
    });
  });

  describe("getTagPageViews", () => {
    it("タグごとのPV数を集計して返す", async () => {
      const pageViews = [
        buildPageView("article", ULID_A1, "2024-01-15", "desktop", null),
        buildPageView("article", ULID_A1, "2024-01-16", "desktop", null),
      ];
      const articles = [
        buildArticle(ULID_A1, "Tagged Article", [ULID_TAG1, ULID_TAG2]),
      ];
      mockLoadCurrentPageViews.mockResolvedValue(pageViews);
      mockLoadAllArticles.mockResolvedValue(articles);

      const { getTagPageViews } = await import("@/actions/analytics");
      const result = await getTagPageViews("30d");

      expect(result).toHaveLength(2);
      expect(result[0].value).toBe(2);
    });
  });

  describe("getContentTypeComparison", () => {
    it("コンテンツタイプごとのPV数を Distribution 配列で返す", async () => {
      const pageViews = [
        buildPageView("article", ULID_A1, "2024-01-15", "desktop", null),
        buildPageView("article", ULID_A2, "2024-01-15", "desktop", null),
        buildPageView("memo", ULID_M1, "2024-01-15", "desktop", null),
      ];
      mockLoadCurrentPageViews.mockResolvedValue(pageViews);

      const { getContentTypeComparison } = await import("@/actions/analytics");
      const result = await getContentTypeComparison("30d");

      expect(result[0].label).toBe("article");
      expect(result[0].value).toBe(2);
    });
  });
});
