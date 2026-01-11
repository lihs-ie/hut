import { describe, it, expect } from "vitest";
import {
  ARTICLE_FRONTMATTER_TEMPLATE,
  stripFrontmatter,
  extractFrontmatterTitle,
  updateFrontmatterTitle,
  articleMatter,
} from "@shared/components/global/matter";

describe("components/global/matter", () => {
  describe("ARTICLE_FRONTMATTER_TEMPLATE", () => {
    it("正しいテンプレート構造を持つ", () => {
      expect(ARTICLE_FRONTMATTER_TEMPLATE).toContain("---\n");
      expect(ARTICLE_FRONTMATTER_TEMPLATE).toContain("title:");
      expect(ARTICLE_FRONTMATTER_TEMPLATE).toContain("excerpt:");
      expect(ARTICLE_FRONTMATTER_TEMPLATE).toContain("slug:");
      expect(ARTICLE_FRONTMATTER_TEMPLATE).toContain("tags: []");
    });
  });

  describe("stripFrontmatter", () => {
    it("frontmatterを除去してコンテンツのみを返す", () => {
      const content = `---
title: テスト
excerpt: 説明
slug: test
tags: []
---

本文です`;

      const result = stripFrontmatter(content);
      expect(result).toBe("本文です");
    });

    it("テンプレートのみの場合は空文字列を返す", () => {
      const result = stripFrontmatter(ARTICLE_FRONTMATTER_TEMPLATE);
      expect(result).toBe("");
    });

    it("テンプレートの後にテキストを追加した場合", () => {
      const content = ARTICLE_FRONTMATTER_TEMPLATE + "入力したテキスト";
      const result = stripFrontmatter(content);
      expect(result).toBe("入力したテキスト");
    });

    it("frontmatterがない場合はそのまま返す", () => {
      const content = "普通のテキスト";
      const result = stripFrontmatter(content);
      expect(result).toBe("普通のテキスト");
    });

    it("閉じタグの後に改行がない場合も動作する", () => {
      const content = `---
title: テスト
excerpt: 説明
slug: test
tags: []
---本文`;

      const result = stripFrontmatter(content);
      expect(result).toBe("本文");
    });

    it("frontmatter内にexcerpt:があってもストリップされる", () => {
      const content = `---
title: テスト
excerpt: これは説明です
slug: test-slug
tags: []
---

ここが本文`;

      const result = stripFrontmatter(content);
      expect(result).toBe("ここが本文");
      expect(result).not.toContain("excerpt:");
    });

    it("実際の入力シナリオ: テンプレート + 改行 + 本文", () => {
      const content = `---
title:
excerpt:
slug:
tags: []
---

テスト本文`;

      const result = stripFrontmatter(content);
      expect(result).toBe("テスト本文");
      expect(result).not.toContain("excerpt:");
      expect(result).not.toContain("title:");
    });

    it("ARTICLE_FRONTMATTER_TEMPLATEの構造を確認", () => {
      // テンプレートが正しい形式か確認
      expect(ARTICLE_FRONTMATTER_TEMPLATE.startsWith("---\n")).toBe(true);
      expect(ARTICLE_FRONTMATTER_TEMPLATE).toContain("\n---\n");
    });

    it("テンプレートに1文字追加した場合", () => {
      const content = ARTICLE_FRONTMATTER_TEMPLATE + "あ";
      const result = stripFrontmatter(content);

      expect(result).toBe("あ");
      expect(result).not.toContain("excerpt");
    });

    it("テンプレートの---直後に入力した場合（改行なし）", () => {
      // ユーザーがテンプレートの最後の改行を消して入力した場合
      const content = `---
title:
excerpt:
slug:
tags: []
---テスト`;

      const result = stripFrontmatter(content);
      expect(result).toBe("テスト");
    });

    it("frontmatterの閉じタグ直前に改行がない場合は不正な形式として扱う", () => {
      // 注: 閉じタグ---の直前には必ず改行が必要
      // この形式は不正なので、frontmatterとして認識されない
      const content = `---
title:
excerpt:
slug:
tags: []---

テスト`;

      const result = stripFrontmatter(content);
      // 不正な形式なのでそのまま返される
      expect(result).toBe(content);
    });
  });

  describe("extractFrontmatterTitle", () => {
    it("frontmatterからタイトルを抽出する", () => {
      const content = `---
title: テストタイトル
excerpt: 説明
slug: test
tags: []
---

本文`;

      const result = extractFrontmatterTitle(content);
      expect(result).toBe("テストタイトル");
    });

    it("タイトルが空の場合は空文字列を返す", () => {
      const content = `---
title:
excerpt: 説明
slug: test
tags: []
---`;

      const result = extractFrontmatterTitle(content);
      // 現在の実装では空タイトルの場合に次の行をキャプチャしてしまう問題がある
      // この問題は別途修正が必要
      expect(result).toBe("");
    });

    it("タイトルにスペースがある場合", () => {
      const content = `---
title: スペースあり
excerpt: 説明
slug: test
tags: []
---`;

      const result = extractFrontmatterTitle(content);
      expect(result).toBe("スペースあり");
    });

    it("frontmatterがない場合はnullを返す", () => {
      const content = "普通のテキスト";
      const result = extractFrontmatterTitle(content);
      expect(result).toBeNull();
    });
  });

  describe("updateFrontmatterTitle", () => {
    it("frontmatter内のタイトルを更新する", () => {
      const content = `---
title: 古いタイトル
excerpt: 説明
slug: test
tags: []
---

本文`;

      const result = updateFrontmatterTitle(content, "新しいタイトル");
      expect(result).toContain("title: 新しいタイトル");
      expect(result).not.toContain("古いタイトル");
    });

    it("空のタイトルを更新する", () => {
      const content = ARTICLE_FRONTMATTER_TEMPLATE;
      const result = updateFrontmatterTitle(content, "入力タイトル");
      expect(result).toContain("title: 入力タイトル");
    });

    it("frontmatterがない場合はそのまま返す", () => {
      const content = "普通のテキスト";
      const result = updateFrontmatterTitle(content, "タイトル");
      expect(result).toBe("普通のテキスト");
    });
  });

  describe("articleMatter", () => {
    it("正しいfrontmatterをパースできる", () => {
      const content = `---
title: テスト記事
excerpt: これはテスト記事の説明です
slug: test-article
tags: []
---

本文です`;

      const result = articleMatter(content);
      expect(result.isOk).toBe(true);
      result.map((parsed) => {
        expect(parsed.data.title).toBe("テスト記事");
        expect(parsed.data.excerpt).toBe("これはテスト記事の説明です");
        expect(parsed.data.slug).toBe("test-article");
        expect(parsed.data.tags).toEqual([]);
      });
    });

    it("タグが設定されている場合もパースできる", () => {
      // タグ識別子はULID形式（26文字）である必要がある
      const tag1 = "01ARZ3NDEKTSV4RRFFQ69G5FAV";
      const tag2 = "01ARZ3NDEKTSV4RRFFQ69G5FAW";
      const content = `---
title: テスト記事
excerpt: 説明
slug: test-article
tags: [${tag1}, ${tag2}]
---

本文`;

      const result = articleMatter(content);
      expect(result.isOk).toBe(true);
      if (result.isOk) {
        const parsed = result.unwrap();
        expect(parsed.data.tags).toHaveLength(2);
        expect(parsed.data.tags).toContain(tag1);
        expect(parsed.data.tags).toContain(tag2);
      }
    });

    it("タイトルが空の場合はエラーを返す", () => {
      const content = `---
title:
excerpt: 説明
slug: test
tags: []
---`;

      const result = articleMatter(content);
      expect(result.isErr).toBe(true);
      result.mapError((error) => {
        expect(error.message).toContain("Invalid article front matter");
      });
    });

    it("excerptが空の場合も成功する", () => {
      // excerptSchemaにmin制約がないため、空文字列は許可される
      const content = `---
title: タイトル
excerpt:
slug: test
tags: []
---`;

      const result = articleMatter(content);
      expect(result.isOk).toBe(true);
    });

    it("slugが空の場合はエラーを返す", () => {
      const content = `---
title: タイトル
excerpt: 説明
slug:
tags: []
---`;

      const result = articleMatter(content);
      expect(result.isErr).toBe(true);
      result.mapError((error) => {
        expect(error.message).toContain("Invalid article front matter");
      });
    });

    it("frontmatterがない場合はエラーを返す", () => {
      const content = "普通のテキスト";

      const result = articleMatter(content);
      expect(result.isErr).toBe(true);
    });

    it("必須フィールドが不足している場合はエラーを返す", () => {
      const content = `---
title: タイトル
---`;

      const result = articleMatter(content);
      expect(result.isErr).toBe(true);
      result.mapError((error) => {
        expect(error.message).toContain("Invalid article front matter");
      });
    });

    it("本文を正しく取得できる", () => {
      const content = `---
title: テスト
excerpt: 説明
slug: test
tags: []
---

これは本文です。
複数行あります。`;

      const result = articleMatter(content);
      expect(result.isOk).toBe(true);
      result.map((parsed) => {
        expect(parsed.content).toContain("これは本文です。");
        expect(parsed.content).toContain("複数行あります。");
      });
    });

    it("エラーメッセージにフィールドと説明が含まれる", () => {
      const content = `---
title:
excerpt: 説明
slug: test
tags: []
---`;

      const result = articleMatter(content);
      expect(result.isErr).toBe(true);
      if (result.isErr) {
        const error = result.unwrapError();
        expect(error.message).toContain("title");
      }
    });
  });
});
