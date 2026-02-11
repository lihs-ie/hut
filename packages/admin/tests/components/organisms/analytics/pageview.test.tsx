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

vi.mock("@shared/components/molecules/chart/bar", () => ({
  BarChartPanel: (props: Record<string, unknown>) => (
    <div data-testid="bar-chart">
      <span data-testid="title">{String(props.title)}</span>
    </div>
  ),
}));

describe("components/organisms/analytics/PageViewTrend", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("PV推移とコンテンツタイプ別PVを並列取得して表示する", async () => {
    const getPageViewTrend = vi.fn().mockResolvedValue([
      { dateKey: "2024-01-15", value: 100 },
      { dateKey: "2024-01-16", value: 200 },
    ]);
    const getContentTypeComparison = vi.fn().mockResolvedValue([
      { label: "記事", value: 500 },
      { label: "メモ", value: 300 },
    ]);

    const { PageViewTrend } = await import(
      "@/app/admin/_components/organisms/analytics/pageview"
    );

    const element = await PageViewTrend({
      getPageViewTrend,
      getContentTypeComparison,
      period: "7d",
    });

    expect(element).toBeDefined();
    expect(getPageViewTrend).toHaveBeenCalledWith("7d");
    expect(getContentTypeComparison).toHaveBeenCalledWith("7d");
  });

  it("空データでもレンダリングが成功する", async () => {
    const getPageViewTrend = vi.fn().mockResolvedValue([]);
    const getContentTypeComparison = vi.fn().mockResolvedValue([]);

    const { PageViewTrend } = await import(
      "@/app/admin/_components/organisms/analytics/pageview"
    );

    const element = await PageViewTrend({
      getPageViewTrend,
      getContentTypeComparison,
      period: "30d",
    });

    expect(element).toBeDefined();
  });

  it("データ取得関数がエラーを投げた場合、例外が伝播する", async () => {
    const getPageViewTrend = vi
      .fn()
      .mockRejectedValue(new Error("fetch error"));
    const getContentTypeComparison = vi.fn().mockResolvedValue([]);

    const { PageViewTrend } = await import(
      "@/app/admin/_components/organisms/analytics/pageview"
    );

    await expect(
      PageViewTrend({
        getPageViewTrend,
        getContentTypeComparison,
        period: "7d",
      })
    ).rejects.toThrow("fetch error");
  });
});
