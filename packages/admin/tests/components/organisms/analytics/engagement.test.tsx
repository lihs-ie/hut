/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("@shared/components/molecules/list/ranking", () => ({
  RankingTable: (props: Record<string, unknown>) => (
    <div data-testid="ranking-table">
      <span data-testid="title">{String(props.title)}</span>
    </div>
  ),
}));

vi.mock("@shared/components/molecules/chart/bar", () => ({
  BarChartPanel: (props: Record<string, unknown>) => (
    <div data-testid="bar-chart">
      <span data-testid="title">{String(props.title)}</span>
    </div>
  ),
}));

describe("components/organisms/analytics/EngagementSection", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("滞在時間ランキングとスクロール深度を並列取得して表示する", async () => {
    const getDwellTimeRanking = vi.fn().mockResolvedValue([
      { label: "記事A", value: 300, subLabel: "/articles/a" },
      { label: "記事B", value: 240, subLabel: "/articles/b" },
    ]);
    const getScrollDepthDistribution = vi.fn().mockResolvedValue([
      { label: "0-25%", value: 100 },
      { label: "25-50%", value: 200 },
      { label: "50-75%", value: 150 },
      { label: "75-100%", value: 50 },
    ]);

    const { EngagementSection } = await import(
      "@/app/admin/_components/organisms/analytics/engagement"
    );

    const element = await EngagementSection({
      getDwellTimeRanking,
      getScrollDepthDistribution,
      period: "7d",
    });

    expect(element).toBeDefined();
    expect(getDwellTimeRanking).toHaveBeenCalledWith("7d");
    expect(getScrollDepthDistribution).toHaveBeenCalledWith("7d");
  });

  it("空データでもレンダリングが成功する", async () => {
    const getDwellTimeRanking = vi.fn().mockResolvedValue([]);
    const getScrollDepthDistribution = vi.fn().mockResolvedValue([]);

    const { EngagementSection } = await import(
      "@/app/admin/_components/organisms/analytics/engagement"
    );

    const element = await EngagementSection({
      getDwellTimeRanking,
      getScrollDepthDistribution,
      period: "30d",
    });

    expect(element).toBeDefined();
  });

  it("データ取得関数がエラーを投げた場合、例外が伝播する", async () => {
    const getDwellTimeRanking = vi
      .fn()
      .mockRejectedValue(new Error("fetch error"));
    const getScrollDepthDistribution = vi.fn().mockResolvedValue([]);

    const { EngagementSection } = await import(
      "@/app/admin/_components/organisms/analytics/engagement"
    );

    await expect(
      EngagementSection({
        getDwellTimeRanking,
        getScrollDepthDistribution,
        period: "7d",
      })
    ).rejects.toThrow("fetch error");
  });
});
