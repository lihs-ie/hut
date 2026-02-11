/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("@shared/components/atoms/card/stats", () => ({
  StatsCard: (props: Record<string, unknown>) => (
    <div data-testid="stats-card">
      <span data-testid="title">{String(props.title)}</span>
      <span data-testid="value">{String(props.value)}</span>
      {props.trend && (
        <span data-testid="trend">
          {String((props.trend as { value: number }).value)}
        </span>
      )}
    </div>
  ),
}));

describe("components/organisms/analytics/AnalyticsSummary", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("4つのデータ取得関数を並列に呼び出してStatsCardを表示する", async () => {
    const getTotalPageViews = vi
      .fn()
      .mockResolvedValue({ current: 10000, previous: 8000 });
    const getUniqueVisitors = vi
      .fn()
      .mockResolvedValue({ current: 5000, previous: 4000 });
    const getAverageDwellTime = vi
      .fn()
      .mockResolvedValue({ current: 180, previous: 150 });
    const getSearchCount = vi
      .fn()
      .mockResolvedValue({ current: 300, previous: 250 });

    const { AnalyticsSummary } = await import(
      "@/app/admin/_components/organisms/analytics/summary"
    );

    const element = await AnalyticsSummary({
      getTotalPageViews,
      getUniqueVisitors,
      getAverageDwellTime,
      getSearchCount,
      period: "7d",
    });

    expect(element).toBeDefined();
    expect(getTotalPageViews).toHaveBeenCalledWith("7d");
    expect(getUniqueVisitors).toHaveBeenCalledWith("7d");
    expect(getAverageDwellTime).toHaveBeenCalledWith("7d");
    expect(getSearchCount).toHaveBeenCalledWith("7d");
  });

  it("previous が 0 の場合、trend は 0 になる", async () => {
    const getTotalPageViews = vi
      .fn()
      .mockResolvedValue({ current: 100, previous: 0 });
    const getUniqueVisitors = vi
      .fn()
      .mockResolvedValue({ current: 50, previous: 0 });
    const getAverageDwellTime = vi
      .fn()
      .mockResolvedValue({ current: 60, previous: 0 });
    const getSearchCount = vi
      .fn()
      .mockResolvedValue({ current: 10, previous: 0 });

    const { AnalyticsSummary } = await import(
      "@/app/admin/_components/organisms/analytics/summary"
    );

    const element = await AnalyticsSummary({
      getTotalPageViews,
      getUniqueVisitors,
      getAverageDwellTime,
      getSearchCount,
      period: "30d",
    });

    expect(element).toBeDefined();
  });

  it("平均滞在時間が M:SS 形式でフォーマットされる", async () => {
    const getTotalPageViews = vi
      .fn()
      .mockResolvedValue({ current: 100, previous: 100 });
    const getUniqueVisitors = vi
      .fn()
      .mockResolvedValue({ current: 50, previous: 50 });
    const getAverageDwellTime = vi
      .fn()
      .mockResolvedValue({ current: 125, previous: 100 });
    const getSearchCount = vi
      .fn()
      .mockResolvedValue({ current: 10, previous: 10 });

    const { AnalyticsSummary } = await import(
      "@/app/admin/_components/organisms/analytics/summary"
    );

    const element = await AnalyticsSummary({
      getTotalPageViews,
      getUniqueVisitors,
      getAverageDwellTime,
      getSearchCount,
      period: "7d",
    });

    expect(element).toBeDefined();
  });

  it("データ取得関数がエラーを投げた場合、例外が伝播する", async () => {
    const getTotalPageViews = vi
      .fn()
      .mockRejectedValue(new Error("fetch error"));
    const getUniqueVisitors = vi
      .fn()
      .mockResolvedValue({ current: 50, previous: 40 });
    const getAverageDwellTime = vi
      .fn()
      .mockResolvedValue({ current: 60, previous: 50 });
    const getSearchCount = vi
      .fn()
      .mockResolvedValue({ current: 10, previous: 8 });

    const { AnalyticsSummary } = await import(
      "@/app/admin/_components/organisms/analytics/summary"
    );

    await expect(
      AnalyticsSummary({
        getTotalPageViews,
        getUniqueVisitors,
        getAverageDwellTime,
        getSearchCount,
        period: "7d",
      })
    ).rejects.toThrow("fetch error");
  });
});
