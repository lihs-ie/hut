/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { ContentEmpty } from "@shared/components/molecules/empty/content";

describe("components/molecules/empty/ContentEmpty", () => {
  describe("デフォルト表示", () => {
    it("タイトルが常に表示される", () => {
      render(<ContentEmpty title="記事がありません" />);

      expect(screen.getByText("記事がありません")).toBeInTheDocument();
    });

    it("descriptionが渡された場合、説明文が表示される", () => {
      render(
        <ContentEmpty
          title="記事がありません"
          description="記事が公開されるとここに表示されます"
        />
      );

      expect(
        screen.getByText("記事が公開されるとここに表示されます")
      ).toBeInTheDocument();
    });

    it("descriptionが渡されない場合、説明文の要素は描画されない", () => {
      const { container } = render(
        <ContentEmpty title="記事がありません" />
      );

      expect(container.querySelector("p")).toBeNull();
    });
  });

  describe("アイコン表示", () => {
    it("アイコンが渡された場合に表示される", () => {
      render(
        <ContentEmpty
          title="テスト"
          description="説明"
          icon={<span data-testid="test-icon">icon</span>}
        />
      );

      expect(screen.getByTestId("test-icon")).toBeInTheDocument();
    });

    it("アイコンが渡されない場合でもクラッシュしない", () => {
      render(<ContentEmpty title="テスト" description="説明" />);

      expect(screen.getByText("テスト")).toBeInTheDocument();
    });
  });

  describe("スタイル", () => {
    it("containerクラスを持つ", () => {
      const { container } = render(
        <ContentEmpty title="テスト" description="説明" />
      );

      const firstChild = container.firstChild;
      if (!(firstChild instanceof HTMLElement)) {
        throw new Error("firstChild is not an HTMLElement");
      }
      expect(firstChild.className).toMatch(/container/);
    });
  });
});
