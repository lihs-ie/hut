import { describe, it, expect } from "vitest";
import { parseImageRemotePatterns } from "../../config/image-remote-pattern";

describe("parseImageRemotePatterns", () => {
  describe("正常系", () => {
    it("単一のHTTPSパターンをパースできる", () => {
      const result = parseImageRemotePatterns(
        "https://firebasestorage.googleapis.com"
      );

      expect(result).toEqual([
        {
          protocol: "https",
          hostname: "firebasestorage.googleapis.com",
        },
      ]);
    });

    it("カンマ区切りの複数パターンをパースできる", () => {
      const result = parseImageRemotePatterns(
        "https://example.com,http://localhost"
      );

      expect(result).toEqual([
        { protocol: "https", hostname: "example.com" },
        { protocol: "http", hostname: "localhost" },
      ]);
    });

    it("前後の空白をトリムする", () => {
      const result = parseImageRemotePatterns(
        "  https://example.com , https://another.com  "
      );

      expect(result).toEqual([
        { protocol: "https", hostname: "example.com" },
        { protocol: "https", hostname: "another.com" },
      ]);
    });

    it("undefinedを渡すと空配列を返す", () => {
      expect(parseImageRemotePatterns(undefined)).toEqual([]);
    });

    it("空文字列を渡すと空配列を返す", () => {
      expect(parseImageRemotePatterns("")).toEqual([]);
    });
  });

  describe("異常系", () => {
    it("ワイルドカードを含むパターンはエラー", () => {
      expect(() => parseImageRemotePatterns("https://**")).toThrow(
        /Wildcard patterns are not allowed/
      );

      expect(() =>
        parseImageRemotePatterns("https://*.example.com")
      ).toThrow(/Wildcard patterns are not allowed/);
    });

    it("複数パターンの中にワイルドカードがあるとエラー", () => {
      expect(() =>
        parseImageRemotePatterns("https://example.com,https://**")
      ).toThrow(/Wildcard patterns are not allowed/);
    });

    it("不正なURL形式はエラー", () => {
      expect(() => parseImageRemotePatterns("not-a-url")).toThrow(
        /Invalid URL format/
      );
    });

    it("非対応プロトコルはエラー", () => {
      expect(() =>
        parseImageRemotePatterns("ftp://example.com")
      ).toThrow(/Unsupported protocol/);
    });
  });
});
