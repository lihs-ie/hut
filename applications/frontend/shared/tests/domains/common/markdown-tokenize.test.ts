import { describe, it, expect } from "vitest";
import {
  tokenizeMarkdown,
  extractCheckableSegments,
} from "@shared/domains/common/markdown";

describe("domains/common/markdown tokenize", () => {
  describe("tokenizeMarkdown", () => {
    describe("フロントマターの除外", () => {
      it("フロントマターをfrontmatterトークンとして分離する", () => {
        const text = "---\ntitle: Test\n---\nHello world";
        const tokens = tokenizeMarkdown(text);

        const frontmatterTokens = tokens.filter(
          (token) => token.type === "frontmatter",
        );
        const textTokens = tokens.filter((token) => token.type === "text");

        expect(frontmatterTokens).toHaveLength(1);
        expect(frontmatterTokens[0].content).toBe("---\ntitle: Test\n---");
        expect(
          textTokens.some((token) => token.content.includes("Hello world")),
        ).toBe(true);
      });

      it("フロントマターがない場合はfrontmatterトークンが生成されない", () => {
        const text = "Hello world";
        const tokens = tokenizeMarkdown(text);

        const frontmatterTokens = tokens.filter(
          (token) => token.type === "frontmatter",
        );
        expect(frontmatterTokens).toHaveLength(0);
      });
    });

    describe("見出し記号の除外", () => {
      it("見出し記号をsyntaxトークンとし、テキスト部分はtextトークンにする", () => {
        const text = "# Hello";
        const tokens = tokenizeMarkdown(text);

        const syntaxTokens = tokens.filter(
          (token) => token.type === "syntax",
        );
        const textTokens = tokens.filter((token) => token.type === "text");

        expect(
          syntaxTokens.some((token) => token.content === "# "),
        ).toBe(true);
        expect(
          textTokens.some((token) => token.content === "Hello"),
        ).toBe(true);
      });

      it("複数レベルの見出しを処理する", () => {
        const text = "## Section\n### Subsection";
        const tokens = tokenizeMarkdown(text);

        const syntaxTokens = tokens.filter(
          (token) => token.type === "syntax",
        );
        expect(
          syntaxTokens.some((token) => token.content === "## "),
        ).toBe(true);
        expect(
          syntaxTokens.some((token) => token.content === "### "),
        ).toBe(true);
      });
    });

    describe("強調記法の除外", () => {
      it("太字のマーカーをsyntaxトークンとし、テキストはtextトークンにする", () => {
        const text = "**bold**";
        const tokens = tokenizeMarkdown(text);

        const syntaxTokens = tokens.filter(
          (token) => token.type === "syntax",
        );
        const textTokens = tokens.filter((token) => token.type === "text");

        expect(
          syntaxTokens.filter((token) => token.content === "**"),
        ).toHaveLength(2);
        expect(
          textTokens.some((token) => token.content === "bold"),
        ).toBe(true);
      });

      it("イタリックのマーカーをsyntaxトークンにする", () => {
        const text = "*italic*";
        const tokens = tokenizeMarkdown(text);

        const syntaxTokens = tokens.filter(
          (token) => token.type === "syntax",
        );
        const textTokens = tokens.filter((token) => token.type === "text");

        expect(
          syntaxTokens.filter((token) => token.content === "*"),
        ).toHaveLength(2);
        expect(
          textTokens.some((token) => token.content === "italic"),
        ).toBe(true);
      });

      it("取り消し線のマーカーをsyntaxトークンにする", () => {
        const text = "~~strikethrough~~";
        const tokens = tokenizeMarkdown(text);

        const syntaxTokens = tokens.filter(
          (token) => token.type === "syntax",
        );
        const textTokens = tokens.filter((token) => token.type === "text");

        expect(
          syntaxTokens.filter((token) => token.content === "~~"),
        ).toHaveLength(2);
        expect(
          textTokens.some((token) => token.content === "strikethrough"),
        ).toBe(true);
      });
    });

    describe("リンク記法", () => {
      it("リンクのテキスト部分はtextトークン、構文部分はsyntaxトークンにする", () => {
        const text = "[click here](https://example.com)";
        const tokens = tokenizeMarkdown(text);

        const syntaxTokens = tokens.filter(
          (token) => token.type === "syntax",
        );
        const textTokens = tokens.filter((token) => token.type === "text");

        expect(
          syntaxTokens.some((token) => token.content === "["),
        ).toBe(true);
        expect(
          syntaxTokens.some((token) => token.content === "]("),
        ).toBe(true);
        expect(
          syntaxTokens.some(
            (token) => token.content === "https://example.com",
          ),
        ).toBe(true);
        expect(
          syntaxTokens.some((token) => token.content === ")"),
        ).toBe(true);
        expect(
          textTokens.some((token) => token.content === "click here"),
        ).toBe(true);
      });
    });

    describe("画像記法", () => {
      it("画像記法全体をsyntaxトークンにする", () => {
        const text = "![alt text](https://example.com/image.png)";
        const tokens = tokenizeMarkdown(text);

        const syntaxTokens = tokens.filter(
          (token) => token.type === "syntax",
        );
        expect(
          syntaxTokens.some(
            (token) =>
              token.content ===
              "![alt text](https://example.com/image.png)",
          ),
        ).toBe(true);
      });
    });

    describe("コードブロック", () => {
      it("コードブロック全体をcodeトークンにする", () => {
        const text = "```\nconsole.log('hello');\n```";
        const tokens = tokenizeMarkdown(text);

        const codeTokens = tokens.filter((token) => token.type === "code");
        expect(codeTokens).toHaveLength(1);
        expect(codeTokens[0].content).toBe(
          "```\nconsole.log('hello');\n```",
        );
      });

      it("言語指定付きコードブロックを処理する", () => {
        const text = "```typescript\nconst x = 1;\n```";
        const tokens = tokenizeMarkdown(text);

        const codeTokens = tokens.filter((token) => token.type === "code");
        expect(codeTokens).toHaveLength(1);
        expect(codeTokens[0].content).toBe(
          "```typescript\nconst x = 1;\n```",
        );
      });
    });

    describe("インラインコード", () => {
      it("インラインコードをcodeトークンにする", () => {
        const text = "Use `console.log` for debugging";
        const tokens = tokenizeMarkdown(text);

        const codeTokens = tokens.filter((token) => token.type === "code");
        const textTokens = tokens.filter((token) => token.type === "text");

        expect(codeTokens).toHaveLength(1);
        expect(codeTokens[0].content).toBe("`console.log`");
        expect(
          textTokens.some((token) => token.content.includes("Use ")),
        ).toBe(true);
        expect(
          textTokens.some((token) =>
            token.content.includes(" for debugging"),
          ),
        ).toBe(true);
      });
    });

    describe("HTMLコメント", () => {
      it("HTMLコメントをsyntaxトークンにする", () => {
        const text = "Hello <!-- comment --> World";
        const tokens = tokenizeMarkdown(text);

        const syntaxTokens = tokens.filter(
          (token) => token.type === "syntax",
        );
        const textTokens = tokens.filter((token) => token.type === "text");

        expect(
          syntaxTokens.some(
            (token) => token.content === "<!-- comment -->",
          ),
        ).toBe(true);
        expect(
          textTokens.some((token) => token.content.includes("Hello")),
        ).toBe(true);
        expect(
          textTokens.some((token) => token.content.includes("World")),
        ).toBe(true);
      });
    });

    describe("複合構文", () => {
      it("テキスト中の複数の構文を処理する", () => {
        const text =
          "This is **bold** and *italic* with `code` and [link](url)";
        const tokens = tokenizeMarkdown(text);

        const textTokens = tokens.filter((token) => token.type === "text");
        const textContent = textTokens
          .map((token) => token.content)
          .join("");

        expect(textContent).toContain("This is ");
        expect(textContent).toContain("bold");
        expect(textContent).toContain(" and ");
        expect(textContent).toContain("italic");
        expect(textContent).toContain("link");
      });
    });

    describe("エッジケース", () => {
      it("空テキストは空配列を返す", () => {
        const tokens = tokenizeMarkdown("");
        expect(tokens).toEqual([]);
      });

      it("構文のみのテキストにはtextトークンが含まれない", () => {
        const text = "```\ncode only\n```";
        const tokens = tokenizeMarkdown(text);

        const textTokens = tokens.filter((token) => token.type === "text");
        expect(textTokens).toHaveLength(0);
      });

      it("テキストのみの場合は全体がtextトークンになる", () => {
        const text = "Just plain text here";
        const tokens = tokenizeMarkdown(text);

        expect(tokens).toHaveLength(1);
        expect(tokens[0].type).toBe("text");
        expect(tokens[0].content).toBe("Just plain text here");
      });

      it("トークンのoffsetとlengthが正しい", () => {
        const text = "Hello **world**";
        const tokens = tokenizeMarkdown(text);

        for (const token of tokens) {
          expect(
            text.substring(token.offset, token.offset + token.length),
          ).toBe(token.content);
        }
      });

      it("全トークンを結合すると元のテキストに一致する", () => {
        const text =
          "# Hello **world**\n\nSome `code` and [link](url)\n\n```\nblock\n```";
        const tokens = tokenizeMarkdown(text);

        const sorted = [...tokens].sort(
          (a, b) => a.offset - b.offset,
        );
        const reconstructed = sorted
          .map((token) => token.content)
          .join("");
        expect(reconstructed).toBe(text);
      });
    });
  });

  describe("extractCheckableSegments", () => {
    it("textトークンのみを抽出する", () => {
      const text = "# Hello **world**";
      const segments = extractCheckableSegments(text);

      const segmentTexts = segments.map((segment) => segment.text);
      expect(segmentTexts.some((text) => text.includes("Hello"))).toBe(true);
      expect(segmentTexts).toContain("world");
    });

    it("構文要素のテキストは含まれない", () => {
      const text = "```\ncode\n```";
      const segments = extractCheckableSegments(text);
      expect(segments).toHaveLength(0);
    });

    it("offsetが元テキスト内の正しい位置を指す", () => {
      const text = "Hello **world**";
      const segments = extractCheckableSegments(text);

      for (const segment of segments) {
        expect(
          text.substring(
            segment.offset,
            segment.offset + segment.text.length,
          ),
        ).toBe(segment.text);
      }
    });

    it("空テキストは空配列を返す", () => {
      const segments = extractCheckableSegments("");
      expect(segments).toEqual([]);
    });

    it("フロントマター付きのMarkdownからテキストのみ抽出する", () => {
      const text = "---\ntitle: Test\n---\n\nHello world";
      const segments = extractCheckableSegments(text);

      const segmentTexts = segments.map((segment) => segment.text);
      expect(segmentTexts.join("")).toContain("Hello world");
      expect(segmentTexts.join("")).not.toContain("title");
    });

    it("リンクのテキスト部分のみ抽出しURLは除外する", () => {
      const text = "Visit [our site](https://example.com) today";
      const segments = extractCheckableSegments(text);

      const combined = segments
        .map((segment) => segment.text)
        .join("");
      expect(combined).toContain("Visit ");
      expect(combined).toContain("our site");
      expect(combined).toContain(" today");
      expect(combined).not.toContain("https://example.com");
    });
  });
});
