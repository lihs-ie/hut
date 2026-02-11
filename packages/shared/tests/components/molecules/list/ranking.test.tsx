/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { RankingTable } from "@shared/components/molecules/list/ranking";

describe("components/molecules/list/RankingTable", () => {
  const sampleItems = [
    { label: "React入門ガイド", value: 320, subLabel: "article" },
    { label: "Next.js Tips", value: 210, subLabel: "article" },
    { label: "TypeScript入門", value: 180, subLabel: "memo" },
    { label: "Rust入門", value: 150, subLabel: "article" },
    { label: "DDD解説", value: 120, subLabel: "memo" },
  ];

  describe("表示", () => {
    it("タイトルが表示される", () => {
      render(
        <RankingTable
          title="コンテンツPVランキング"
          items={sampleItems}
        />
      );

      expect(screen.getByText("コンテンツPVランキング")).toBeInTheDocument();
    });

    it("説明文が表示される", () => {
      render(
        <RankingTable
          title="コンテンツPVランキング"
          description="閲覧数の多い記事・メモ"
          items={sampleItems}
        />
      );

      expect(
        screen.getByText("閲覧数の多い記事・メモ")
      ).toBeInTheDocument();
    });

    it("全てのアイテムのラベルが表示される", () => {
      render(
        <RankingTable
          title="ランキング"
          items={sampleItems}
        />
      );

      expect(screen.getByText("React入門ガイド")).toBeInTheDocument();
      expect(screen.getByText("Next.js Tips")).toBeInTheDocument();
      expect(screen.getByText("TypeScript入門")).toBeInTheDocument();
      expect(screen.getByText("Rust入門")).toBeInTheDocument();
      expect(screen.getByText("DDD解説")).toBeInTheDocument();
    });

    it("ランク番号が表示される", () => {
      render(
        <RankingTable
          title="ランキング"
          items={sampleItems}
        />
      );

      expect(screen.getByText("1")).toBeInTheDocument();
      expect(screen.getByText("2")).toBeInTheDocument();
      expect(screen.getByText("3")).toBeInTheDocument();
    });
  });

  describe("制限", () => {
    it("limitで表示件数を制限できる", () => {
      render(
        <RankingTable
          title="ランキング"
          items={sampleItems}
          limit={3}
        />
      );

      expect(screen.getByText("React入門ガイド")).toBeInTheDocument();
      expect(screen.getByText("Next.js Tips")).toBeInTheDocument();
      expect(screen.getByText("TypeScript入門")).toBeInTheDocument();
      expect(screen.queryByText("Rust入門")).not.toBeInTheDocument();
      expect(screen.queryByText("DDD解説")).not.toBeInTheDocument();
    });

    it("limitが未指定の場合はデフォルトで10件まで表示される", () => {
      const manyItems = Array.from({ length: 15 }, (_, index) => ({
        label: `Item ${index + 1}`,
        value: 100 - index,
      }));

      render(
        <RankingTable title="ランキング" items={manyItems} />
      );

      expect(screen.getByText("Item 1")).toBeInTheDocument();
      expect(screen.getByText("Item 10")).toBeInTheDocument();
      expect(screen.queryByText("Item 11")).not.toBeInTheDocument();
    });
  });

  describe("フォーマッター", () => {
    it("valueFormatterでカスタムフォーマットが適用される", () => {
      render(
        <RankingTable
          title="ランキング"
          items={[{ label: "記事A", value: 320 }]}
          valueFormatter={(value: number) => `${value} PV`}
        />
      );

      expect(screen.getByText("320 PV")).toBeInTheDocument();
    });
  });

  describe("空状態", () => {
    it("アイテムが空の場合はランキング行が表示されない", () => {
      const { container } = render(
        <RankingTable title="ランキング" items={[]} />
      );

      const rows = container.querySelectorAll('[class*="rank"]');
      expect(rows).toHaveLength(0);
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(
        <RankingTable title="ランキング" items={sampleItems} />
      );

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });
  });
});
