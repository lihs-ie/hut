/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import { ArticleEdit } from "@shared/components/templates/article/edit";
import { ARTICLE_FRONTMATTER_TEMPLATE } from "@shared/components/global/matter";
import { PublishStatus } from "@shared/domains/common";

// next/navigation のモック
vi.mock("next/navigation", () => ({
  useRouter: vi.fn(() => ({
    push: vi.fn(),
    back: vi.fn(),
  })),
}));

const mockUploadImage = vi.fn();

describe("ArticleEdit コンポーネント - 改行保持テスト", () => {
  const mockTags = [
    {
      identifier: "tag-1",
      name: "TypeScript",
      logo: "",
      timeline: { createdAt: new Date(), updatedAt: new Date() },
    },
  ];

  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("ARTICLE_FRONTMATTER_TEMPLATE", () => {
    it("テンプレートに改行が含まれている", () => {
      console.log("=== ARTICLE_FRONTMATTER_TEMPLATE ===");
      console.log(JSON.stringify(ARTICLE_FRONTMATTER_TEMPLATE));
      console.log("=====================================");

      expect(ARTICLE_FRONTMATTER_TEMPLATE).toContain("\n");
      expect(ARTICLE_FRONTMATTER_TEMPLATE).toMatch(/^---\n/);
      expect(ARTICLE_FRONTMATTER_TEMPLATE).toMatch(/\n---\n/);
    });
  });

  describe("初期値", () => {
    it("新規作成時にテキストエリアにテンプレートが設定される", () => {
      const mockPersist = vi.fn();

      render(<ArticleEdit persist={mockPersist} tags={mockTags} uploadImage={mockUploadImage} />);

      const textarea = screen.getByPlaceholderText("マークダウンで記述");
      const value = (textarea as HTMLTextAreaElement).value;

      console.log("=== テキストエリアの初期値 ===");
      console.log(JSON.stringify(value));
      console.log("==============================");

      expect(value).toContain("\n");
      expect(value).toBe(ARTICLE_FRONTMATTER_TEMPLATE);
    });

    it("既存記事編集時に改行が保持される", () => {
      const mockPersist = vi.fn();
      const contentWithNewlines = `---
title: テスト記事
excerpt: これはテストです
slug: test-article
tags: []
---

# 見出し

本文です。`;

      const initialArticle = {
        identifier: "01HTEST123456789ABCDEF",
        title: "テスト記事",
        content: contentWithNewlines,
        excerpt: "これはテストです",
        slug: "test-article",
        status: PublishStatus.DRAFT,
        tags: [],
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      };

      render(
        <ArticleEdit
          initial={initialArticle}
          persist={mockPersist}
          tags={mockTags}
          uploadImage={mockUploadImage}
        />
      );

      const textarea = screen.getByPlaceholderText("マークダウンで記述");
      const value = (textarea as HTMLTextAreaElement).value;

      console.log("=== 既存記事のテキストエリア値 ===");
      console.log(JSON.stringify(value));
      console.log("==================================");

      expect(value).toContain("\n");
      expect(value).toBe(contentWithNewlines);
    });
  });

  describe("コンテンツ変更", () => {
    it("テキストエリアに入力した改行が保持される", () => {
      const mockPersist = vi.fn();

      render(<ArticleEdit persist={mockPersist} tags={mockTags} uploadImage={mockUploadImage} />);

      const textarea = screen.getByPlaceholderText("マークダウンで記述");
      const newContent = `---
title: 新しい記事
excerpt: テスト
slug: new-article
tags: []
---

# 新しい見出し

新しい本文です。`;

      fireEvent.change(textarea, { target: { value: newContent } });

      const updatedValue = (textarea as HTMLTextAreaElement).value;

      console.log("=== 変更後のテキストエリア値 ===");
      console.log(JSON.stringify(updatedValue));
      console.log("================================");

      expect(updatedValue).toContain("\n");
      expect(updatedValue).toBe(newContent);
    });
  });

  describe("保存処理", () => {
    it("保存時に persist 関数に改行付きコンテンツが渡される", async () => {
      const mockPersist = vi.fn().mockResolvedValue(undefined);

      render(<ArticleEdit persist={mockPersist} tags={mockTags} uploadImage={mockUploadImage} />);

      // タイトルを入力（保存ボタンを有効化するため）
      const titleInput = screen.getByPlaceholderText("タイトルを入力");
      fireEvent.change(titleInput, { target: { value: "テスト記事" } });

      // コンテンツを入力
      const textarea = screen.getByPlaceholderText("マークダウンで記述");
      const contentWithNewlines = `---
title: テスト記事
excerpt: テスト概要
slug: test-slug
tags: []
---

# 見出し

本文です。`;

      fireEvent.change(textarea, { target: { value: contentWithNewlines } });

      // 保存ボタンをクリック
      const saveButton = screen.getByText("下書き保存");
      fireEvent.click(saveButton);

      // persist が呼ばれるのを待つ
      await waitFor(
        () => {
          expect(mockPersist).toHaveBeenCalled();
        },
        { timeout: 5000 }
      );

      // persist に渡された引数を検証
      const persistCall = mockPersist.mock.calls[0][0];

      console.log("=== persist に渡された content ===");
      console.log(JSON.stringify(persistCall.content));
      console.log("===================================");

      expect(persistCall.content).toContain("\n");
      expect(persistCall.content).toBe(contentWithNewlines);
    });
  });
});
