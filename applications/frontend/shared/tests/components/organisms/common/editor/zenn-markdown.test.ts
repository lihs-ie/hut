/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import {
  parseMessageBox,
  parseAccordion,
  parseCodeBlockFilename,
  parseImageWidth,
} from "@shared/components/organisms/common/editor/zenn-markdown";

describe("zenn-markdown", () => {
  describe("parseMessageBox", () => {
    it(":::message ブロックをパースする", () => {
      const input = ":::message\nこれはメッセージです\n:::";
      const result = parseMessageBox(input);
      expect(result).toContain('class="message"');
      expect(result).toContain("これはメッセージです");
    });

    it(":::message alert ブロックをパースする", () => {
      const input = ":::message alert\nこれはアラートです\n:::";
      const result = parseMessageBox(input);
      expect(result).toContain('class="message alert"');
      expect(result).toContain("これはアラートです");
    });

    it("メッセージボックスがない場合はそのまま返す", () => {
      const input = "通常のテキスト";
      const result = parseMessageBox(input);
      expect(result).toBe("通常のテキスト");
    });

    it("複数のメッセージボックスをパースする", () => {
      const input =
        ":::message\n最初のメッセージ\n:::\n\n:::message alert\n二番目のアラート\n:::";
      const result = parseMessageBox(input);
      expect(result).toContain('class="message"');
      expect(result).toContain("最初のメッセージ");
      expect(result).toContain('class="message alert"');
      expect(result).toContain("二番目のアラート");
    });
  });

  describe("parseAccordion", () => {
    it(":::details ブロックをパースする", () => {
      const input = ":::details タイトル\n中身のテキスト\n:::";
      const result = parseAccordion(input);
      expect(result).toContain("<details>");
      expect(result).toContain("<summary>タイトル</summary>");
      expect(result).toContain("中身のテキスト");
    });

    it("アコーディオンがない場合はそのまま返す", () => {
      const input = "通常のテキスト";
      const result = parseAccordion(input);
      expect(result).toBe("通常のテキスト");
    });
  });

  describe("parseCodeBlockFilename", () => {
    it("ファイル名付きコードブロックをパースする", () => {
      const input = "```typescript:example.ts\nconst x = 1;\n```";
      const result = parseCodeBlockFilename(input);
      expect(result).toContain("example.ts");
      expect(result).toContain("const x = 1;");
    });

    it("ファイル名なしのコードブロックはそのまま返す", () => {
      const input = "```typescript\nconst x = 1;\n```";
      const result = parseCodeBlockFilename(input);
      expect(result).toBe("```typescript\nconst x = 1;\n```");
    });
  });

  describe("parseImageWidth", () => {
    it("幅指定付き画像をパースする", () => {
      const input = "![alt](https://example.com/image.png =250x)";
      const result = parseImageWidth(input);
      expect(result).toContain('width="250"');
      expect(result).toContain('src="https://example.com/image.png"');
    });

    it("幅指定なし画像はそのまま返す", () => {
      const input = "![alt](https://example.com/image.png)";
      const result = parseImageWidth(input);
      expect(result).toBe("![alt](https://example.com/image.png)");
    });
  });
});
