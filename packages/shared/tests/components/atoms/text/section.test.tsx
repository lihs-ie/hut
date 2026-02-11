/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { SectionHeader } from "@shared/components/atoms/text/section";

describe("components/atoms/text/SectionHeader", () => {
  describe("表示", () => {
    it("タイトルが表示される", () => {
      render(<SectionHeader title="PV推移" />);

      expect(screen.getByText("PV推移")).toBeInTheDocument();
    });

    it("説明が表示される", () => {
      render(
        <SectionHeader
          title="PV推移"
          description="日別のページビュー数の推移"
        />
      );

      expect(
        screen.getByText("日別のページビュー数の推移")
      ).toBeInTheDocument();
    });

    it("説明が未指定の場合は説明要素が表示されない", () => {
      const { container } = render(<SectionHeader title="PV推移" />);

      const description = container.querySelector('[class*="description"]');
      expect(description).not.toBeInTheDocument();
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(<SectionHeader title="テスト" />);

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });

    it("タイトルにtitleクラスが適用される", () => {
      render(<SectionHeader title="テスト" />);

      const title = screen.getByText("テスト");
      expect(title.className).toContain("title");
    });

    it("説明にdescriptionクラスが適用される", () => {
      render(<SectionHeader title="テスト" description="説明文" />);

      const description = screen.getByText("説明文");
      expect(description.className).toContain("description");
    });
  });
});
