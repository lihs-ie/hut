/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("@shared/components/molecules/chart/line", () => ({
  LineChartPanel: (props: Record<string, unknown>) => (
    <div data-testid="line-chart">
      <span data-testid="title">{String(props.title)}</span>
    </div>
  ),
}));

vi.mock("@shared/components/molecules/list/ranking", () => ({
  RankingTable: (props: Record<string, unknown>) => (
    <div data-testid="ranking-table">
      <span data-testid="title">{String(props.title)}</span>
    </div>
  ),
}));

describe("components/organisms/analytics/SearchAnalyticsSection", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("検索回数推移、キーワードランキング、ゼロヒットキーワードを並列取得して表示する", async () => {
    const getSearchCountTrend = vi.fn().mockResolvedValue([
      { dateKey: "2024-01-15", value: 50 },
      { dateKey: "2024-01-16", value: 80 },
    ]);
    const getSearchKeywordRanking = vi.fn().mockResolvedValue([
      { label: "React", value: 30 },
      { label: "Next.js", value: 20 },
    ]);
    const getZeroHitKeywords = vi.fn().mockResolvedValue([
      { label: "unknown", value: 5 },
      { label: "xyz", value: 3 },
    ]);

    const { SearchAnalyticsSection } = await import(
      "@/app/admin/_components/organisms/analytics/search"
    );

    const element = await SearchAnalyticsSection({
      getSearchCountTrend,
      getSearchKeywordRanking,
      getZeroHitKeywords,
      period: "7d",
    });

    expect(element).toBeDefined();
    expect(getSearchCountTrend).toHaveBeenCalledWith("7d");
    expect(getSearchKeywordRanking).toHaveBeenCalledWith("7d");
    expect(getZeroHitKeywords).toHaveBeenCalledWith("7d");
  });

  it("空データでもレンダリングが成功する", async () => {
    const getSearchCountTrend = vi.fn().mockResolvedValue([]);
    const getSearchKeywordRanking = vi.fn().mockResolvedValue([]);
    const getZeroHitKeywords = vi.fn().mockResolvedValue([]);

    const { SearchAnalyticsSection } = await import(
      "@/app/admin/_components/organisms/analytics/search"
    );

    const element = await SearchAnalyticsSection({
      getSearchCountTrend,
      getSearchKeywordRanking,
      getZeroHitKeywords,
      period: "30d",
    });

    expect(element).toBeDefined();
  });

  it("データ取得関数がエラーを投げた場合、例外が伝播する", async () => {
    const getSearchCountTrend = vi
      .fn()
      .mockRejectedValue(new Error("fetch error"));
    const getSearchKeywordRanking = vi.fn().mockResolvedValue([]);
    const getZeroHitKeywords = vi.fn().mockResolvedValue([]);

    const { SearchAnalyticsSection } = await import(
      "@/app/admin/_components/organisms/analytics/search"
    );

    await expect(
      SearchAnalyticsSection({
        getSearchCountTrend,
        getSearchKeywordRanking,
        getZeroHitKeywords,
        period: "7d",
      })
    ).rejects.toThrow("fetch error");
  });
});
