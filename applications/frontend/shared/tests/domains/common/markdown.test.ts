import { describe, it, expect } from "vitest";
import { extractImageUrls } from "@shared/domains/common/markdown";

describe("domains/common/markdown", () => {
  describe("extractImageUrls", () => {
    it("単一の画像URLを抽出する", () => {
      const markdown = "テスト ![alt](https://example.com/image.png) テスト";
      const urls = extractImageUrls(markdown);
      expect(urls).toEqual(new Set(["https://example.com/image.png"]));
    });

    it("複数の画像URLを抽出する", () => {
      const markdown = `
![image1](https://example.com/a.png)
テキスト
![image2](https://example.com/b.webp)
`;
      const urls = extractImageUrls(markdown);
      expect(urls).toEqual(
        new Set([
          "https://example.com/a.png",
          "https://example.com/b.webp",
        ]),
      );
    });

    it("画像がない場合は空のSetを返す", () => {
      const markdown = "これは画像を含まないテキストです";
      const urls = extractImageUrls(markdown);
      expect(urls.size).toBe(0);
    });

    it("アップロード中のplaceholderは除外しない（URLとして抽出される）", () => {
      const markdown = "![uploading...](placeholder-abc123)";
      const urls = extractImageUrls(markdown);
      expect(urls).toEqual(new Set(["placeholder-abc123"]));
    });

    it("通常のリンクは抽出しない", () => {
      const markdown = "[リンクテキスト](https://example.com)";
      const urls = extractImageUrls(markdown);
      expect(urls.size).toBe(0);
    });

    it("空のalt textでも抽出する", () => {
      const markdown = "![](https://example.com/image.png)";
      const urls = extractImageUrls(markdown);
      expect(urls).toEqual(new Set(["https://example.com/image.png"]));
    });

    it("空のmarkdownは空のSetを返す", () => {
      const urls = extractImageUrls("");
      expect(urls.size).toBe(0);
    });

    it("重複するURLは一つにまとめる", () => {
      const markdown = `
![a](https://example.com/same.png)
![b](https://example.com/same.png)
`;
      const urls = extractImageUrls(markdown);
      expect(urls.size).toBe(1);
      expect(urls).toEqual(new Set(["https://example.com/same.png"]));
    });
  });
});
