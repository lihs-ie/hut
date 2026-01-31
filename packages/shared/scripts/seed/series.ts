/**
 * シリーズシードデータ
 */

import { createDocument, SERIES_IDS, TAG_IDS } from "./common";

export async function seedSeries(): Promise<void> {
  console.log("\n--- Creating Series ---");

  const now = new Date();

  const seriesData = [
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
      chapters: [
        {
          title: "第1章: Rustの基礎",
          slug: "chapter-1-rust-basics",
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
          timeline: {
            createdAt: now,
            updatedAt: now,
          },
        },
        {
          title: "第2章: メモリ管理",
          slug: "chapter-2-memory-management",
          content: `# 第2章: メモリ管理

Rustのメモリ管理の仕組みを理解します。

## スタックとヒープ

データがどこに格納されるかを理解することは重要です。

## ライフタイム

参照の有効期間を明示的に指定する仕組みです。
`,
          timeline: {
            createdAt: now,
            updatedAt: now,
          },
        },
      ],
    },
  ];

  for (const series of seriesData) {
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

// シリーズデータをエクスポート（search-tokenで使用）
export const SERIES_DATA = [
  {
    id: "series1",
    title: "Rustで学ぶシステムプログラミング",
    excerpt:
      "Rustを使ってシステムプログラミングの基礎を学ぶシリーズです。メモリ管理、並行処理、ネットワークプログラミングなどを扱います。",
    tags: ["rust"],
  },
];
