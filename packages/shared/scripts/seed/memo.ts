/**
 * メモシードデータ
 */

import { createDocument, MEMO_IDS, TAG_IDS } from "./common";

export async function seedMemos(): Promise<void> {
  console.log("\n--- Creating Memos ---");

  const now = new Date();
  const yesterday = new Date(now.getTime() - 24 * 60 * 60 * 1000);

  const memos = [
    {
      id: MEMO_IDS.memo1,
      title: "Go言語のTips",
      slug: "go-tips",
      tags: [TAG_IDS.go],
      status: "published",
      entries: [
        {
          text: "Go言語ではエラーは値として扱う。`error`インターフェースを実装すれば独自エラー型を作れる。",
          createdAt: yesterday,
        },
        {
          text: "goroutineは軽量スレッド。`go`キーワードで簡単に並行処理ができる。",
          createdAt: now,
        },
      ],
    },
    {
      id: MEMO_IDS.memo2,
      title: "TypeScript設定メモ",
      slug: "typescript-config",
      tags: [TAG_IDS.typescript],
      status: "draft",
      entries: [
        {
          text: "`strict: true`は必須。型安全性を最大限に活用するため。",
          createdAt: yesterday,
        },
        {
          text: "`noUncheckedIndexedAccess`を有効にすると配列アクセスも安全になる。",
          createdAt: now,
        },
        {
          text: "パスエイリアスは`paths`と`baseUrl`で設定。ビルドツールの設定も忘れずに。",
          createdAt: now,
        },
      ],
    },
  ];

  for (const memo of memos) {
    await createDocument(
      "memos",
      memo.id,
      {
        identifier: memo.id,
        title: memo.title,
        slug: memo.slug,
        tags: memo.tags,
        entries: memo.entries,
        timeline: {
          createdAt: yesterday,
          updatedAt: now,
        },
        version: 1,
        status: memo.status,
      },
      { useTimestamp: true },
    );
  }
}

// メモデータをエクスポート（search-tokenで使用）
export const MEMOS_DATA = [
  {
    id: "memo1",
    title: "Go言語のTips",
    excerpt: "Go言語ではエラーは値として扱う。goroutineは軽量スレッド。",
    tags: ["go"],
  },
  {
    id: "memo2",
    title: "TypeScript設定メモ",
    excerpt:
      "strict: trueは必須。noUncheckedIndexedAccessを有効にすると配列アクセスも安全になる。",
    tags: ["typescript"],
  },
];
