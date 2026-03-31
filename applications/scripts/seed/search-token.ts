import {
  createDocument,
  createSubDocument,
  generateNgrams,
  TAG_IDS,
  ARTICLE_IDS,
  SERIES_IDS,
  CHAPTER_IDS,
  MEMO_IDS,
} from "./common";

type ContentType = "article" | "memo" | "series" | "chapter";

type ContentReference = {
  type: ContentType;
  id: string;
  title: string;
  excerpt: string;
  tags: (keyof typeof TAG_IDS)[];
};

const ALL_CONTENTS: ContentReference[] = [
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
  {
    type: "series",
    id: SERIES_IDS.series1,
    title: "Rustで学ぶシステムプログラミング",
    excerpt:
      "Rustを使ってシステムプログラミングの基礎を学ぶシリーズです。メモリ管理、並行処理、ネットワークプログラミングなどを扱います。",
    tags: ["rust"],
  },
  {
    type: "series",
    id: SERIES_IDS.series2,
    title: "実践TypeScript設計パターン",
    excerpt:
      "TypeScriptの型システムを最大限に活用し、保守性の高いアプリケーションを設計するためのパターンを解説するシリーズです。",
    tags: ["typescript", "react"],
  },
  {
    type: "chapter",
    id: CHAPTER_IDS.series1Chapter1,
    title: "Rustの基礎：所有権とライフタイム",
    excerpt: "Rustの所有権システムとライフタイムの基本を学びます。",
    tags: ["rust"],
  },
  {
    type: "chapter",
    id: CHAPTER_IDS.series1Chapter2,
    title: "メモリ管理の仕組み",
    excerpt: "Rustのメモリ管理の仕組みを詳しく解説します。",
    tags: ["rust"],
  },
  {
    type: "chapter",
    id: CHAPTER_IDS.series2Chapter1,
    title: "型駆動開発入門",
    excerpt: "TypeScriptの型システムを活用した開発手法を紹介します。",
    tags: ["typescript"],
  },
  {
    type: "memo",
    id: MEMO_IDS.memo1,
    title: "Go言語のTips",
    excerpt: "Go言語ではエラーは値として扱う。goroutineは軽量スレッド。",
    tags: ["go"],
  },
];

function encodeTokenIdentifier(identifier: string): string {
  return identifier;
}

const contentTokenMap = new Map<string, string[]>();

function trackContentToken(
  contentType: ContentType,
  contentIdentifier: string,
  tokenIdentifier: string,
): void {
  const key = `${contentType}:${contentIdentifier}`;

  if (!contentTokenMap.has(key)) {
    contentTokenMap.set(key, []);
  }

  const tokens = contentTokenMap.get(key)!;

  if (!tokens.includes(tokenIdentifier)) {
    tokens.push(tokenIdentifier);
  }
}

async function createTagTokens(): Promise<void> {
  console.log("\n  --- Creating Tag Tokens ---");

  const now = new Date();

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

  for (const [tagId, contents] of tagContentMap) {
    const tokenIdentifier = `tag:${tagId}`;
    const encodedTokenId = encodeTokenIdentifier(tokenIdentifier);

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
          score: 10.0,
          updatedAt: now,
        },
        { useTimestamp: true },
      );

      trackContentToken(content.type, content.id, tokenIdentifier);
    }
  }
}

async function createNgramTokens(): Promise<void> {
  console.log("\n  --- Creating Ngram Tokens ---");

  const now = new Date();

  const ngramContentMap = new Map<
    string,
    Array<{ content: ContentReference; score: number }>
  >();

  for (const content of ALL_CONTENTS) {
    const searchText = [content.title, content.excerpt].join(" ");
    const ngrams = generateNgrams(searchText);

    for (const ngram of ngrams) {
      if (!ngramContentMap.has(ngram)) {
        ngramContentMap.set(ngram, []);
      }
      const inTitle = content.title.toLowerCase().includes(ngram);
      const score = inTitle ? 5.0 : 2.0;

      ngramContentMap.get(ngram)!.push({ content, score });
    }
  }

  let count = 0;
  const maxTokens = 50;

  for (const [ngram, contentRefs] of ngramContentMap) {
    if (count >= maxTokens) break;

    const tokenIdentifier = `ngram:${ngram}`;
    const encodedTokenId = encodeTokenIdentifier(tokenIdentifier);

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

      trackContentToken(content.type, content.id, tokenIdentifier);
    }

    count++;
  }

  console.log(`  Created ${count} ngram tokens (limited to ${maxTokens})`);
}

async function createContentTokenIndex(): Promise<void> {
  console.log("\n  --- Creating Content Token Index ---");

  const now = new Date();

  for (const [contentKey, tokens] of contentTokenMap) {
    await createDocument(
      "content-token-index",
      contentKey,
      {
        tokens,
        updatedAt: now,
      },
      { useTimestamp: true },
    );
  }

  console.log(`  Created ${contentTokenMap.size} content token index entries`);
}

export async function seedSearchTokens(): Promise<void> {
  console.log("\n--- Creating Search Tokens ---");

  contentTokenMap.clear();
  await createTagTokens();
  await createNgramTokens();
  await createContentTokenIndex();
}
