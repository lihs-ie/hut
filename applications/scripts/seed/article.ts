/**
 * 記事シードデータ
 */

import { createDocument, ARTICLE_IDS, TAG_IDS } from "./common";

export async function seedArticles(): Promise<void> {
  console.log("\n--- Creating Articles ---");

  const now = new Date();
  const oneDayMs = 24 * 60 * 60 * 1000;

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
      createdAt: new Date(now.getTime() - 5 * oneDayMs),
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
      createdAt: new Date(now.getTime() - 4 * oneDayMs),
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
      createdAt: new Date(now.getTime() - 3 * oneDayMs),
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
      createdAt: new Date(now.getTime() - 2 * oneDayMs),
    },
    {
      id: ARTICLE_IDS.article5,
      title: "Mermaid図を使った技術ドキュメント",
      slug: "mermaid-diagram-test",
      excerpt: "Mermaid記法を使ってフローチャートやシーケンス図を描画するテストです。",
      content: `---
title: Mermaid図を使った技術ドキュメント
excerpt: Mermaid記法を使ってフローチャートやシーケンス図を描画するテストです。
slug: mermaid-diagram-test
tags: [${TAG_IDS.typescript}]
---

# Mermaid図を使った技術ドキュメント

Mermaid記法を使うことで、コード内に図を埋め込むことができます。

## フローチャート

\`\`\`mermaid
flowchart TD
    A[開始] --> B{条件分岐}
    B -->|Yes| C[処理A]
    B -->|No| D[処理B]
    C --> E[終了]
    D --> E
\`\`\`

## まとめ

Mermaid記法を使えば、テキストベースで図を管理できます。
`,
      image:
        "https://images.unsplash.com/photo-1516116216624-53e697fedbea?w=800",
      status: "published",
      tags: [TAG_IDS.typescript],
      createdAt: new Date(now.getTime() - 1 * oneDayMs),
    },
    {
      id: ARTICLE_IDS.article6,
      title: "Mermaid図 全サンプル集",
      slug: "mermaid-all-diagrams",
      excerpt:
        "Mermaidがサポートする主要な図種（フローチャート、シーケンス図、クラス図、状態遷移図、ER図、ガントチャート、円グラフ、Git グラフ）を一覧で紹介します。",
      content: `---
title: Mermaid図 全サンプル集
excerpt: Mermaidがサポートする主要な図種を一覧で紹介します。
slug: mermaid-all-diagrams
tags: [${TAG_IDS.typescript}]
---

# Mermaid図 全サンプル集

Mermaidがサポートする主要な図種のサンプルをまとめました。

## フローチャート

\`\`\`mermaid
flowchart TD
    A[開始] --> B{条件分岐}
    B -->|Yes| C[処理A]
    B -->|No| D[処理B]
    C --> E[終了]
    D --> E
\`\`\`

## シーケンス図

\`\`\`mermaid
sequenceDiagram
    participant Client
    participant API
    participant DB
    Client->>API: リクエスト送信
    API->>DB: クエリ実行
    DB-->>API: 結果返却
    API-->>Client: レスポンス返却
    Client->>API: データ更新
    API->>DB: UPDATE実行
    DB-->>API: 成功
    API-->>Client: 200 OK
\`\`\`

## クラス図

\`\`\`mermaid
classDiagram
    class Animal {
        +String name
        +int age
        +makeSound() void
    }
    class Dog {
        +String breed
        +fetch() void
    }
    class Cat {
        +bool isIndoor
        +purr() void
    }
    Animal <|-- Dog
    Animal <|-- Cat
\`\`\`

## 状態遷移図

\`\`\`mermaid
stateDiagram-v2
    [*] --> Draft
    Draft --> Review : 提出
    Review --> Approved : 承認
    Review --> Draft : 差し戻し
    Approved --> Published : 公開
    Published --> [*]
\`\`\`

## ER図

\`\`\`mermaid
erDiagram
    USER ||--o{ ARTICLE : writes
    USER {
        string id PK
        string name
        string email
    }
    ARTICLE ||--o{ TAG : has
    ARTICLE {
        string id PK
        string title
        string content
        string authorId FK
    }
    TAG {
        string id PK
        string name
    }
\`\`\`

## ガントチャート

\`\`\`mermaid
gantt
    title プロジェクトスケジュール
    dateFormat YYYY-MM-DD
    section 設計
        要件定義      :a1, 2026-01-01, 14d
        基本設計      :a2, after a1, 10d
    section 開発
        フロントエンド :b1, after a2, 20d
        バックエンド   :b2, after a2, 25d
    section テスト
        結合テスト    :c1, after b2, 10d
        受入テスト    :c2, after c1, 7d
\`\`\`

## 円グラフ

\`\`\`mermaid
pie title 使用言語の割合
    "TypeScript" : 40
    "Rust" : 25
    "Go" : 20
    "Python" : 15
\`\`\`

## Git グラフ

\`\`\`mermaid
gitGraph
    commit
    branch develop
    checkout develop
    commit
    commit
    branch feature
    checkout feature
    commit
    commit
    checkout develop
    merge feature
    checkout main
    merge develop
    commit tag:"v1.0"
\`\`\`

## まとめ

以上がMermaidの主要な図種です。テキストベースで図を管理でき、バージョン管理との相性も良いのが特徴です。
`,
      image:
        "https://images.unsplash.com/photo-1516116216624-53e697fedbea?w=800",
      status: "published",
      tags: [TAG_IDS.typescript],
      createdAt: now,
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
      images: [],
      timeline: {
        createdAt: article.createdAt,
        updatedAt: article.createdAt,
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
  {
    id: "article5",
    title: "Mermaid図を使った技術ドキュメント",
    excerpt: "Mermaid記法を使ってフローチャートやシーケンス図を描画するテストです。",
    tags: ["typescript"],
  },
  {
    id: "article6",
    title: "Mermaid図 全サンプル集",
    excerpt:
      "Mermaidがサポートする主要な図種を一覧で紹介します。",
    tags: ["typescript"],
  },
];
