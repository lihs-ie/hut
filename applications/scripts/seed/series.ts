import { createDocument, SERIES_IDS, CHAPTER_IDS, TAG_IDS } from "./common";

const CHAPTER_DATA = [
  {
    id: CHAPTER_IDS.series1Chapter1,
    title: "第1章: Rustの基礎",
    slug: "chapter-1-rust-basics",
    images: [],
    status: "published",
    content: `# 第1章: Rustの基礎

Rustの基本的な文法と特徴について学びます。

## 変数と不変性

Rustでは変数はデフォルトで不変です。

\`\`\`rust
let x = 5; // 不変
let mut y = 5; // 可変
\`\`\`

## 所有権

Rustの最も特徴的な機能が所有権システムです。
`,
  },
  {
    id: CHAPTER_IDS.series1Chapter2,
    title: "第2章: メモリ管理",
    slug: "chapter-2-memory-management",
    images: [],
    status: "published",
    content: `# 第2章: メモリ管理

Rustのメモリ管理の仕組みを理解します。

## スタックとヒープ

データがどこに格納されるかを理解することは重要です。

## ライフタイム

参照の有効期間を明示的に指定する仕組みです。
`,
  },
  {
    id: CHAPTER_IDS.series2Chapter1,
    title: "第1章: 型駆動開発入門",
    slug: "chapter-1-type-driven-development",
    images: [],
    status: "published",
    content: `# 第1章: 型駆動開発入門

型を先に設計し、実装を型に従わせるアプローチを学びます。

## Branded Types

プリミティブ型に意味を持たせるパターンです。

\`\`\`typescript
type UserId = string & { readonly brand: unique symbol };
type Email = string & { readonly brand: unique symbol };
\`\`\`

## Discriminated Union

状態をユニオン型で表現し、網羅的なパターンマッチを実現します。
`,
  },
  {
    id: CHAPTER_IDS.series2Chapter2,
    title: "第2章: Result型によるエラーハンドリング",
    slug: "chapter-2-result-type-error-handling",
    images: [],
    status: "published",
    content: `# 第2章: Result型によるエラーハンドリング

例外をスローする代わりにResult型でエラーを表現するパターンを学びます。

## なぜResult型か

例外は型安全ではなく、呼び出し側がエラーを見逃す原因になります。

## 実装

\`\`\`typescript
type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };
\`\`\`
`,
  },
  {
    id: CHAPTER_IDS.series2Chapter3,
    title: "第3章: 依存性注入と関数合成",
    slug: "chapter-3-dependency-injection",
    images: [],
    status: "published",
    content: `# 第3章: 依存性注入と関数合成

テスタビリティと柔軟性を両立する設計パターンを学びます。

## カリー化による依存性注入

高階関数を使い、依存を外部から注入します。

\`\`\`typescript
const createUserService = (repository: UserRepository) => (logger: Logger) => ({
  find: (id: UserId) => repository.find(id),
});
\`\`\`
`,
  },
  {
    id: CHAPTER_IDS.series3Chapter1,
    title: "第1章: プロジェクト構成とApp Router",
    slug: "chapter-1-project-setup",
    images: [],
    status: "draft",
    content: `# 第1章: プロジェクト構成とApp Router

Next.js App Routerの基本構成とプロジェクトセットアップを学びます。

## ディレクトリ構成

App Routerではファイルシステムベースのルーティングを採用しています。

## Server Components と Client Components

デフォルトはServer Componentです。クライアント側の対話が必要な場合のみClient Componentを使います。
`,
  },
];

const SERIES_DATA_INTERNAL = [
  {
    id: SERIES_IDS.series1,
    title: "Rustで学ぶシステムプログラミング",
    slug: "rust-system-programming",
    subTitle: "低レイヤーからRustを理解する",
    description:
      "Rustを使ってシステムプログラミングの基礎を学ぶシリーズです。メモリ管理、並行処理、ネットワークプログラミングなどを扱います。",
    cover:
      "https://images.unsplash.com/photo-1515879218367-8466d910aaa4?w=800",
    tags: [TAG_IDS.rust],
    status: "published",
    chapters: [CHAPTER_IDS.series1Chapter1, CHAPTER_IDS.series1Chapter2],
  },
  {
    id: SERIES_IDS.series2,
    title: "実践TypeScript設計パターン",
    slug: "typescript-design-patterns",
    subTitle: "型システムを活用した堅牢な設計",
    description:
      "TypeScriptの型システムを最大限に活用し、保守性の高いアプリケーションを設計するためのパターンを解説するシリーズです。",
    cover:
      "https://images.unsplash.com/photo-1516116216624-53e697fedbea?w=800",
    tags: [TAG_IDS.typescript, TAG_IDS.react],
    status: "published",
    chapters: [
      CHAPTER_IDS.series2Chapter1,
      CHAPTER_IDS.series2Chapter2,
      CHAPTER_IDS.series2Chapter3,
    ],
  },
  {
    id: SERIES_IDS.series3,
    title: "Next.jsで作るフルスタックアプリケーション",
    slug: "nextjs-fullstack-application",
    subTitle: null,
    description:
      "Next.js App Routerを使ってフルスタックなWebアプリケーションを構築する実践シリーズです。認証、データベース、デプロイまで網羅します。",
    cover: null,
    tags: [TAG_IDS.nextjs, TAG_IDS.typescript, TAG_IDS.react],
    status: "draft",
    chapters: [CHAPTER_IDS.series3Chapter1],
  },
];

export async function seedSeries(): Promise<void> {
  console.log("\n--- Creating Chapters ---");

  const now = new Date();

  for (const chapter of CHAPTER_DATA) {
    await createDocument(
      "chapters",
      chapter.id,
      {
        identifier: chapter.id,
        title: chapter.title,
        slug: chapter.slug,
        content: chapter.content,
        images: chapter.images,
        status: chapter.status,
        timeline: {
          createdAt: now,
          updatedAt: now,
        },
        version: 1,
      },
      { useTimestamp: true },
    );
  }

  console.log("\n--- Creating Series ---");

  for (const series of SERIES_DATA_INTERNAL) {
    await createDocument(
      "series",
      series.id,
      {
        identifier: series.id,
        title: series.title,
        slug: series.slug,
        subTitle: series.subTitle,
        description: series.description,
        cover: series.cover,
        tags: series.tags,
        chapters: series.chapters,
        status: series.status,
        timeline: {
          createdAt: now,
          updatedAt: now,
        },
        version: 1,
      },
      { useTimestamp: true },
    );
  }
}

export const SERIES_DATA = [
  {
    id: "series1",
    title: "Rustで学ぶシステムプログラミング",
    excerpt:
      "Rustを使ってシステムプログラミングの基礎を学ぶシリーズです。メモリ管理、並行処理、ネットワークプログラミングなどを扱います。",
    tags: ["rust"],
  },
  {
    id: "series2",
    title: "実践TypeScript設計パターン",
    excerpt:
      "TypeScriptの型システムを最大限に活用し、保守性の高いアプリケーションを設計するためのパターンを解説するシリーズです。",
    tags: ["typescript", "react"],
  },
  {
    id: "series3",
    title: "Next.jsで作るフルスタックアプリケーション",
    excerpt:
      "Next.js App Routerを使ってフルスタックなWebアプリケーションを構築する実践シリーズです。認証、データベース、デプロイまで網羅します。",
    tags: ["nextjs", "typescript", "react"],
  },
];
