/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render } from "@testing-library/react";

vi.mock("next/image", () => ({
  __esModule: true,
  default: (imageProps: Record<string, unknown>) => {
    const { priority, ...rest } = imageProps;
    return (
      <img
        {...(rest as React.ImgHTMLAttributes<HTMLImageElement>)}
        {...(priority ? { fetchPriority: "high" as const } : {})}
      />
    );
  },
}));

import { ContentImage } from "@shared/components/atoms/image/content";

describe("components/atoms/image/ContentImage", () => {
  const defaultProps = { src: "/test-image.png", alt: "テスト画像" };

  const renderImage = () => {
    const { container } = render(<ContentImage {...defaultProps} />);
    const image = container.querySelector("img");
    return image;
  };

  describe("props の受け渡し", () => {
    it("src と alt が next/image の Image に正しく渡される", () => {
      const image = renderImage();

      expect(image).toHaveAttribute("src", defaultProps.src);
      expect(image).toHaveAttribute("alt", defaultProps.alt);
    });
  });

  describe("レスポンシブ設定", () => {
    it("sizes 属性にモバイルとデスクトップの値が設定されている", () => {
      const image = renderImage();
      const sizes = image?.getAttribute("sizes") ?? "";

      expect(sizes).toContain("100vw");
      expect(sizes).toContain("800px");
    });

    it("レスポンシブなインラインスタイルが適用されている", () => {
      const image = renderImage();

      expect(image?.style.maxWidth).toBe("100%");
      expect(image?.style.height).toBe("auto");
    });
  });

  describe("CSS クラス", () => {
    it("container クラスが適用されている", () => {
      const image = renderImage();

      expect(image?.className).toContain("container");
    });
  });

  describe("priority prop", () => {
    it("priority: true を渡すと fetchpriority='high' になる", () => {
      const { container } = render(
        <ContentImage src="/test-image.png" alt="テスト" priority />
      );
      const image = container.querySelector("img");

      expect(image).toHaveAttribute("fetchpriority", "high");
    });

    it("priority を渡さない場合 fetchpriority='high' にならない", () => {
      const image = renderImage();

      expect(image).not.toHaveAttribute("fetchpriority", "high");
    });
  });
});
