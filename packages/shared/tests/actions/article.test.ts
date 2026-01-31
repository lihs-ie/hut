/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { PublishStatus } from "@shared/domains/common";
import { validateArticle } from "@shared/domains/articles";
import { ulid } from "ulid";

// ワークフローのテスト用
vi.mock("@shared/providers/workflows/article", () => ({
  ArticleWorkflowProvider: {
    persist: vi.fn(),
  },
}));

describe("Article 保存処理 - 改行保持テスト", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("validateArticle", () => {
    it("改行を含むコンテンツが正しくバリデートされる", () => {
      const contentWithNewlines = `---
title: テスト記事
excerpt: テスト概要
slug: test-slug
tags: []
---

# 見出し

本文です。`;

      const unvalidatedArticle = {
        identifier: ulid(),
        title: "テスト記事",
        content: contentWithNewlines,
        excerpt: "テスト概要",
        slug: "test-slug",
        status: PublishStatus.DRAFT,
        tags: [],
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      };

      const result = validateArticle(unvalidatedArticle);

      console.log("=== validateArticle 入力 ===");
      console.log(JSON.stringify(unvalidatedArticle.content));
      console.log("============================");

      console.log("=== validateArticle 結果 ===");
      console.log("isOk:", result.isOk);
      console.log("result:", JSON.stringify(result, null, 2));
      console.log("==============================");

      if (!result.isOk) {
        result.match({
          ok: () => {},
          err: (error) => {
            console.log("=== validateArticle エラー ===");
            console.log(JSON.stringify(error, null, 2));
            console.log("==============================");
          },
        });
      }

      expect(result.isOk).toBe(true);

      result.match({
        ok: (article) => {
          console.log("=== validateArticle 出力 ===");
          console.log(JSON.stringify(article.content));
          console.log("============================");

          expect(article.content).toContain("\n");
          expect(article.content).toBe(contentWithNewlines);
        },
        err: () => {
          // 既に上でエラー出力済み
        },
      });
    });
  });

  describe("JSON シリアライズ", () => {
    it("JSON.stringify で改行が保持される", () => {
      const contentWithNewlines = `---
title: テスト
---

# 見出し`;

      const data = { content: contentWithNewlines };
      const json = JSON.stringify(data);
      const parsed = JSON.parse(json);

      console.log("=== JSON シリアライズ前 ===");
      console.log(JSON.stringify(contentWithNewlines));
      console.log("=== JSON シリアライズ後 ===");
      console.log(JSON.stringify(parsed.content));
      console.log("===========================");

      expect(parsed.content).toContain("\n");
      expect(parsed.content).toBe(contentWithNewlines);
    });
  });
});
