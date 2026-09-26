import { describe, expect, it, vi } from "vitest";
import { pageViewSchema } from "@shared/domains/analytics/page-view";
import { engagementRecordSchema } from "@shared/domains/analytics/engagement";

const { searchPageViews, searchEngagement } = vi.hoisted(() => ({
  searchPageViews: vi.fn(),
  searchEngagement: vi.fn(),
}));

vi.mock("@/providers/infrastructure/analytics", () => ({
  AdminAnalyticsRepositoryProvider: {
    pageView: { search: searchPageViews },
    engagementRecord: { search: searchEngagement },
  },
}));

vi.mock("@/providers/infrastructure/articles", () => ({
  AdminArticleRepositoryProvider: { firebase: { search: vi.fn() } },
}));

vi.mock("@/providers/infrastructure/tag", () => ({
  AdminTagRepositoryProvider: { firebase: { search: vi.fn() } },
}));

const article = "01JMABCDEF0123456789ABCDE1";
const memo = "01JMABCDEF0123456789ABCDE2";

const pageView = (type: string, content: string) => pageViewSchema.parse({
  identifier: {
    reference: { type, content },
    dateKey: "2026-09-26",
    sessionKey: crypto.randomUUID(),
  },
  referrer: { raw: null },
  deviceType: "desktop",
  createdAt: new Date(),
});

const engagement = (type: string, content: string) =>
  engagementRecordSchema.parse({
    identifier: {
      reference: { type, content },
      dateKey: "2026-09-26",
      sessionKey: crypto.randomUUID(),
    },
    dwellTime: 30,
    scrollDepth: 50,
    createdAt: new Date(),
    updatedAt: new Date(),
  });

describe("Article-only analytics loader", () => {
  it("現在・前期間の PV から記事以外を除外する", async () => {
    searchPageViews.mockReturnValue({
      unwrap: async () => [pageView("article", article), pageView("memo", memo)],
    });

    const { loadCurrentPageViews, loadPreviousPageViews } = await import(
      "@/actions/analytics/loader"
    );

    for (const load of [loadCurrentPageViews, loadPreviousPageViews]) {
      const records = await load("30d");
      expect(records).toHaveLength(1);
      expect(records[0].identifier.reference.type).toBe("article");
    }
  });

  it("現在・前期間の滞在記録から記事以外を除外する", async () => {
    searchEngagement.mockReturnValue({
      unwrap: async () => [engagement("article", article), engagement("memo", memo)],
    });

    const { loadCurrentEngagement, loadPreviousEngagement } = await import(
      "@/actions/analytics/loader"
    );

    for (const load of [loadCurrentEngagement, loadPreviousEngagement]) {
      const records = await load("30d");
      expect(records).toHaveLength(1);
      expect(records[0].identifier.reference.type).toBe("article");
    }
  });
});
