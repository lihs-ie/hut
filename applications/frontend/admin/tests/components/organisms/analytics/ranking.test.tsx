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

describe("components/organisms/analytics/ContentRanking", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("コンテンツランキングを取得して表示する", async () => {
    const getContentRanking = vi.fn().mockResolvedValue([
      { label: "記事A", value: 500, subLabel: "/articles/a" },
      { label: "記事B", value: 300, subLabel: "/articles/b" },
    ]);

    const { ContentRanking } = await import(
      "@/app/admin/_components/organisms/analytics/ranking"
    );

    const element = await ContentRanking({
      getContentRanking,
      period: "7d",
    });

    expect(element).toBeDefined();
    expect(getContentRanking).toHaveBeenCalledWith("7d");
  });

  it("空のランキングデータでもレンダリングが成功する", async () => {
    const getContentRanking = vi.fn().mockResolvedValue([]);

    const { ContentRanking } = await import(
      "@/app/admin/_components/organisms/analytics/ranking"
    );

    const element = await ContentRanking({
      getContentRanking,
      period: "30d",
    });

    expect(element).toBeDefined();
  });

  it("データ取得関数がエラーを投げた場合、例外が伝播する", async () => {
    const getContentRanking = vi
      .fn()
      .mockRejectedValue(new Error("fetch error"));

    const { ContentRanking } = await import(
      "@/app/admin/_components/organisms/analytics/ranking"
    );

    await expect(
      ContentRanking({
        getContentRanking,
        period: "7d",
      })
    ).rejects.toThrow("fetch error");
  });
});
