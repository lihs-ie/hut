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

describe("components/organisms/analytics/TagPageViewSection", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("タグ別PVデータを取得して表示する", async () => {
    const getTagPageViews = vi.fn().mockResolvedValue([
      { label: "React", value: 500 },
      { label: "TypeScript", value: 400 },
      { label: "Next.js", value: 300 },
    ]);

    const { TagPageViewSection } = await import(
      "@/app/admin/_components/organisms/analytics/tag"
    );

    const element = await TagPageViewSection({
      getTagPageViews,
      period: "7d",
    });

    expect(element).toBeDefined();
    expect(getTagPageViews).toHaveBeenCalledWith("7d");
  });

  it("空データでもレンダリングが成功する", async () => {
    const getTagPageViews = vi.fn().mockResolvedValue([]);

    const { TagPageViewSection } = await import(
      "@/app/admin/_components/organisms/analytics/tag"
    );

    const element = await TagPageViewSection({
      getTagPageViews,
      period: "30d",
    });

    expect(element).toBeDefined();
  });

  it("データ取得関数がエラーを投げた場合、例外が伝播する", async () => {
    const getTagPageViews = vi
      .fn()
      .mockRejectedValue(new Error("fetch error"));

    const { TagPageViewSection } = await import(
      "@/app/admin/_components/organisms/analytics/tag"
    );

    await expect(
      TagPageViewSection({
        getTagPageViews,
        period: "7d",
      })
    ).rejects.toThrow("fetch error");
  });
});
