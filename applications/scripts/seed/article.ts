/**
 * 記事シードデータ
 */

import { createDocument, ARTICLE_IDS, TAG_IDS } from "./common";

export async function seedArticles(): Promise<void> {
  console.log("\n--- Creating Articles ---");

  const now = new Date();

  const articles = [
    {
      id: ARTICLE_IDS.article1,
      title: "TypeScriptで型安全なコードを書く",
      slug: "typescript-type-safe-code",
      excerpt:
        "TypeScriptを使って型安全なコードを書くためのベストプラクティスを紹介します。",
      content: `---
title: TypeScriptで型安全なコードを書く
excerpt: TypeScriptを使って型安全なコードを書くためのベストプラクティスを紹介します。
slug: typescript-type-safe-code
tags: [${TAG_IDS.typescript}]
---

# TypeScriptで型安全なコードを書く

TypeScriptは、JavaScriptに型システムを追加した言語です。

## 型の基本

\`\`\`typescript
const name: string = "TypeScript";
const version: number = 5.0;
const isAwesome: boolean = true;
\`\`\`

## ジェネリクス

ジェネリクスを使うことで、再利用可能な型安全なコードを書けます。

\`\`\`typescript
function identity<T>(arg: T): T {
  return arg;
}
\`\`\`
`,
      image:
        "https://images.unsplash.com/photo-1516116216624-53e697fedbea?w=800",
      status: "published",
      tags: [TAG_IDS.typescript],
    },
    {
      id: ARTICLE_IDS.article2,
      title: "Reactコンポーネント設計パターン",
      slug: "react-component-patterns",
      excerpt:
        "Reactで再利用可能なコンポーネントを設計するためのパターンを解説します。",
      content: `---
title: Reactコンポーネント設計パターン
excerpt: Reactで再利用可能なコンポーネントを設計するためのパターンを解説します。
slug: react-component-patterns
tags: [${TAG_IDS.react}, ${TAG_IDS.typescript}]
---

# Reactコンポーネント設計パターン

Reactでコンポーネントを設計する際の主要なパターンを紹介します。

## Compound Components

親子関係を持つコンポーネント群を作成するパターンです。

## Render Props

関数を子要素として渡すパターンです。

## Custom Hooks

ロジックを再利用可能なフックとして抽出するパターンです。
`,
      image:
        "https://images.unsplash.com/photo-1633356122544-f134324a6cee?w=800",
      status: "published",
      tags: [TAG_IDS.react, TAG_IDS.typescript],
    },
    {
      id: ARTICLE_IDS.article3,
      title: "Next.js App Routerの使い方",
      slug: "nextjs-app-router",
      excerpt:
        "Next.js 13以降で導入されたApp Routerの基本的な使い方を解説します。",
      content: `---
title: Next.js App Routerの使い方
excerpt: Next.js 13以降で導入されたApp Routerの基本的な使い方を解説します。
slug: nextjs-app-router
tags: [${TAG_IDS.nextjs}, ${TAG_IDS.react}, ${TAG_IDS.typescript}]
---

# Next.js App Routerの使い方

Next.js 13で導入されたApp Routerは、新しいルーティングシステムです。

## ファイルベースルーティング

\`app\`ディレクトリ内のフォルダ構造がURLパスに対応します。

## Server Components

デフォルトでServer Componentsが使用されます。

## Loading UI

\`loading.tsx\`ファイルでローディング状態を定義できます。
`,
      image:
        "https://images.unsplash.com/photo-1618477388954-7852f32655ec?w=800",
      status: "draft",
      tags: [TAG_IDS.nextjs, TAG_IDS.react, TAG_IDS.typescript],
    },
    {
      id: ARTICLE_IDS.article4,
      title: "Markdownで画像を表示するテスト",
      slug: "markdown-image-test",
      excerpt: "Markdown記法で文中に画像を挿入する方法をテストします。",
      content: `---
title: Markdownで画像を表示するテスト
excerpt: Markdown記法で文中に画像を挿入する方法をテストします。
slug: markdown-image-test
tags: [${TAG_IDS.typescript}, ${TAG_IDS.react}]
---

# Markdownで画像を表示するテスト

この記事では、Markdown記法を使って文中に画像を挿入する方法をテストします。

## 画像の挿入

文中に画像を挿入することができます。以下はサンプル画像です。

![コードを書いている様子](https://images.unsplash.com/photo-1461749280684-dccba630e2f6?w=800)

画像の後にもテキストを続けることができます。

## コードブロックとの共存

画像とコードブロックを同じ記事内で使用できます。

\`\`\`typescript
const message: string = "画像とコードの共存テスト";
console.log(message);
\`\`\`

## まとめ

Markdown記法を使えば、テキスト・画像・コードを組み合わせた記事を作成できます。
`,
      image:
        "https://images.unsplash.com/photo-1461749280684-dccba630e2f6?w=800",
      status: "published",
      tags: [TAG_IDS.typescript, TAG_IDS.react],
    },
  ];

  for (const article of articles) {
    await createDocument("articles", article.id, {
      identifier: article.id,
      title: article.title,
      slug: article.slug,
      excerpt: article.excerpt,
      content: article.content,
      image: article.image,
      status: article.status,
      tags: article.tags,
      timeline: {
        createdAt: now,
        updatedAt: now,
      },
      version: 1,
    });
  }
}

// 記事データをエクスポート（search-tokenで使用）
export const ARTICLES_DATA = [
  {
    id: "article1",
    title: "TypeScriptで型安全なコードを書く",
    excerpt:
      "TypeScriptを使って型安全なコードを書くためのベストプラクティスを紹介します。",
    tags: ["typescript"],
  },
  {
    id: "article2",
    title: "Reactコンポーネント設計パターン",
    excerpt:
      "Reactで再利用可能なコンポーネントを設計するためのパターンを解説します。",
    tags: ["react", "typescript"],
  },
  {
    id: "article3",
    title: "Next.js App Routerの使い方",
    excerpt:
      "Next.js 13以降で導入されたApp Routerの基本的な使い方を解説します。",
    tags: ["nextjs", "react", "typescript"],
  },
  {
    id: "article4",
    title: "Markdownで画像を表示するテスト",
    excerpt: "Markdown記法で文中に画像を挿入する方法をテストします。",
    tags: ["typescript", "react"],
  },
];
