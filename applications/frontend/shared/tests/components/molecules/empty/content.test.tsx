/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { ContentEmpty } from "@shared/components/molecules/empty/content";

describe("components/molecules/empty/ContentEmpty", () => {
  describe("デフォルト表示", () => {
    it("タイトルと説明文が表示される", () => {
      render(
        <ContentEmpty title="記事がありません" description="記事が公開されるとここに表示されます" />
      );

      expect(screen.getByText("記事がありません")).toBeInTheDocument();
      expect(
        screen.getByText("記事が公開されるとここに表示されます")
      ).toBeInTheDocument();
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

      const className = (container.firstChild as HTMLElement).className;
      expect(className).toMatch(/container/);
    });
  });
});
