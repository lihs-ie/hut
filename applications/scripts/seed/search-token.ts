/**
 * SearchTokenシードデータ
 *
 * SearchTokenは検索の逆インデックスとして機能し、
 * 各トークン（TagまたはNGram）がどのコンテンツを参照しているかをマップします。
 */

import {
  createDocument,
  createSubDocument,
  generateNgrams,
  TAG_IDS,
  ARTICLE_IDS,
  SERIES_IDS,
  MEMO_IDS,
} from "./common";

// コンテンツタイプ
type ContentType = "article" | "memo" | "series";

// コンテンツ参照情報
type ContentReference = {
  type: ContentType;
  id: string;
  title: string;
  excerpt: string;
  tags: (keyof typeof TAG_IDS)[];
};

// 全コンテンツのメタデータ
const ALL_CONTENTS: ContentReference[] = [
  // 記事
  {
    type: "article",
    id: ARTICLE_IDS.article1,
    title: "TypeScriptで型安全なコードを書く",
    excerpt:
      "TypeScriptを使って型安全なコードを書くためのベストプラクティスを紹介します。",
    tags: ["typescript"],
  },
  {
    type: "article",
    id: ARTICLE_IDS.article2,
    title: "Reactコンポーネント設計パターン",
    excerpt:
      "Reactで再利用可能なコンポーネントを設計するためのパターンを解説します。",
    tags: ["react", "typescript"],
  },
  {
    type: "article",
    id: ARTICLE_IDS.article3,
    title: "Next.js App Routerの使い方",
    excerpt:
      "Next.js 13以降で導入されたApp Routerの基本的な使い方を解説します。",
    tags: ["nextjs", "react", "typescript"],
  },
  // シリーズ
  {
    type: "series",
    id: SERIES_IDS.series1,
    title: "Rustで学ぶシステムプログラミング",
    excerpt:
      "Rustを使ってシステムプログラミングの基礎を学ぶシリーズです。メモリ管理、並行処理、ネットワークプログラミングなどを扱います。",
    tags: ["rust"],
  },
  // メモ（publishedのみ - draftは検索インデックスに含めない）
  {
    type: "memo",
    id: MEMO_IDS.memo1,
    title: "Go言語のTips",
    excerpt: "Go言語ではエラーは値として扱う。goroutineは軽量スレッド。",
    tags: ["go"],
  },
  // memo2 "TypeScript設定メモ" は status: "draft" のため検索インデックスに含めない
];

// トークン識別子（Firestore document IDとしてそのまま使用）
// Note: インフラ層と同じ形式（エンコードなし）を使用
function encodeTokenIdentifier(identifier: string): string {
  return identifier;
}

// タグトークンを作成
async function createTagTokens(): Promise<void> {
  console.log("\n  --- Creating Tag Tokens ---");

  const now = new Date();

  // タグごとにコンテンツをグループ化
  const tagContentMap = new Map<string, ContentReference[]>();

  for (const content of ALL_CONTENTS) {
    for (const tagKey of content.tags) {
      const tagId = TAG_IDS[tagKey];
      if (!tagContentMap.has(tagId)) {
        tagContentMap.set(tagId, []);
      }
      tagContentMap.get(tagId)!.push(content);
    }
  }

  // 各タグに対してトークンを作成
  for (const [tagId, contents] of tagContentMap) {
    const tokenIdentifier = `tag:${tagId}`;
    const encodedTokenId = encodeTokenIdentifier(tokenIdentifier);

    // トークンドキュメントを作成
    // Note: インフラ層ではcreatedAt/updatedAtをFirestore Timestampとして保存
    await createDocument(
      "search-tokens",
      encodedTokenId,
      {
        identifier: tokenIdentifier,
        type: "tag",
        value: tagId,
        createdAt: now,
        updatedAt: now,
      },
      { useTimestamp: true },
    );

    // 参照サブコレクションを作成
    for (const content of contents) {
      const refId = `${content.type}:${content.id}`;

      await createSubDocument(
        "search-tokens",
        encodedTokenId,
        "refs",
        refId,
        {
          identifier: {
            type: content.type,
            content: content.id,
          },
          score: 10.0, // タグマッチは高スコア
          updatedAt: now,
        },
        { useTimestamp: true },
      );
    }
  }
}

// Ngramトークンを作成
async function createNgramTokens(): Promise<void> {
  console.log("\n  --- Creating Ngram Tokens ---");

  const now = new Date();

  // Ngramごとにコンテンツをグループ化
  const ngramContentMap = new Map<
    string,
    Array<{ content: ContentReference; score: number }>
  >();

  for (const content of ALL_CONTENTS) {
    // タイトルと概要からテキストを結合
    const searchText = [content.title, content.excerpt].join(" ");
    const ngrams = generateNgrams(searchText);

    for (const ngram of ngrams) {
      if (!ngramContentMap.has(ngram)) {
        ngramContentMap.set(ngram, []);
      }
      // タイトルに含まれるNgramは高スコア
      const inTitle = content.title.toLowerCase().includes(ngram);
      const score = inTitle ? 5.0 : 2.0;

      ngramContentMap.get(ngram)!.push({ content, score });
    }
  }

  // 各Ngramに対してトークンを作成（最初の50個のみ、テスト用）
  let count = 0;
  const maxTokens = 50;

  for (const [ngram, contentRefs] of ngramContentMap) {
    if (count >= maxTokens) break;

    const tokenIdentifier = `ngram:${ngram}`;
    const encodedTokenId = encodeTokenIdentifier(tokenIdentifier);

    // トークンドキュメントを作成
    // Note: インフラ層ではcreatedAt/updatedAtをFirestore Timestampとして保存
    await createDocument(
      "search-tokens",
      encodedTokenId,
      {
        identifier: tokenIdentifier,
        type: "ngram",
        value: ngram,
        createdAt: now,
        updatedAt: now,
      },
      { useTimestamp: true },
    );

    // 参照サブコレクションを作成
    for (const { content, score } of contentRefs) {
      const refId = `${content.type}:${content.id}`;

      await createSubDocument(
        "search-tokens",
        encodedTokenId,
        "refs",
        refId,
        {
          identifier: {
            type: content.type,
            content: content.id,
          },
          score,
          updatedAt: now,
        },
        { useTimestamp: true },
      );
    }

    count++;
  }

  console.log(`  Created ${count} ngram tokens (limited to ${maxTokens})`);
}

export async function seedSearchTokens(): Promise<void> {
  console.log("\n--- Creating Search Tokens ---");

  await createTagTokens();
  await createNgramTokens();
}
