/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("@shared/components/molecules/chart/bar", () => ({
  BarChartPanel: (props: Record<string, unknown>) => (
    <div data-testid="bar-chart">
      <span data-testid="title">{String(props.title)}</span>
    </div>
  ),
}));

vi.mock("@shared/components/molecules/chart/pie", () => ({
  PieChartPanel: (props: Record<string, unknown>) => (
    <div data-testid="pie-chart">
      <span data-testid="title">{String(props.title)}</span>
    </div>
  ),
}));

describe("components/organisms/analytics/AccessSourceSection", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("リファラーとデバイス統計を並列取得して表示する", async () => {
    const getReferrerRanking = vi.fn().mockResolvedValue([
      { label: "Google", value: 400 },
      { label: "Direct", value: 200 },
    ]);
    const getDeviceDistribution = vi.fn().mockResolvedValue([
      { label: "Desktop", value: 600 },
      { label: "Mobile", value: 400 },
    ]);

    const { AccessSourceSection } = await import(
      "@/app/admin/_components/organisms/analytics/source"
    );

    const element = await AccessSourceSection({
      getReferrerRanking,
      getDeviceDistribution,
      period: "7d",
    });

    expect(element).toBeDefined();
    expect(getReferrerRanking).toHaveBeenCalledWith("7d");
    expect(getDeviceDistribution).toHaveBeenCalledWith("7d");
  });

  it("空データでもレンダリングが成功する", async () => {
    const getReferrerRanking = vi.fn().mockResolvedValue([]);
    const getDeviceDistribution = vi.fn().mockResolvedValue([]);

    const { AccessSourceSection } = await import(
      "@/app/admin/_components/organisms/analytics/source"
    );

    const element = await AccessSourceSection({
      getReferrerRanking,
      getDeviceDistribution,
      period: "30d",
    });

    expect(element).toBeDefined();
  });

  it("データ取得関数がエラーを投げた場合、例外が伝播する", async () => {
    const getReferrerRanking = vi
      .fn()
      .mockRejectedValue(new Error("fetch error"));
    const getDeviceDistribution = vi.fn().mockResolvedValue([]);

    const { AccessSourceSection } = await import(
      "@/app/admin/_components/organisms/analytics/source"
    );

    await expect(
      AccessSourceSection({
        getReferrerRanking,
        getDeviceDistribution,
        period: "7d",
      })
    ).rejects.toThrow("fetch error");
  });
});
