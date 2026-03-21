import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ChapterPresenter } from "@shared/components/organisms/series/chapter";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  ChapterSlugMold,
  SeriesSlugMold,
} from "../../../../../support/molds/domains/series";

const meta = {
  component: ChapterPresenter,
} satisfies Meta<typeof ChapterPresenter>;

export default meta;

const chapterContent = `## 概要

Next.js 15とReact 19を使用したWebアプリケーション開発における、
基本的なルールとディレクトリ構成について解説します。

### ディレクトリ構成の基本方針

App Routerを前提とした場合、以下のようなディレクトリ構成を推奨します。

\`\`\`
app/
├── (marketing)/
│   ├── page.tsx
│   └── about/
│       └── page.tsx
├── (dashboard)/
│   ├── layout.tsx
│   └── dashboard/
│       └── page.tsx
├── api/
│   └── users/
│       └── route.ts
└── layout.tsx
\`\`\`

### 命名規則

コンポーネントやファイルの命名には一貫性を持たせることが重要です。
以下の規則に従うことを推奨します。

- コンポーネントファイル: PascalCase (例: UserProfile.tsx)
- ユーティリティ関数: camelCase (例: formatDate.ts)
- 定数: UPPER_SNAKE_CASE (例: API_BASE_URL)
`;

const chapterSlug = Forger(ChapterSlugMold).forge({ value: "chapter-01" });

const allChapters = [
  Forger(ChapterMold).forge({
    title: "基本ルールとディレクトリ構成",
    slug: chapterSlug,
    content: chapterContent,
  }),
  Forger(ChapterMold).forgeWithSeed(2, { title: "コンポーネント設計" }),
  Forger(ChapterMold).forgeWithSeed(3, { title: "データ取得" }),
  Forger(ChapterMold).forgeWithSeed(4, { title: "データ更新" }),
  Forger(ChapterMold).forgeWithSeed(5, { title: "状態管理" }),
  Forger(ChapterMold).forgeWithSeed(6, { title: "キャッシュ戦略" }),
  Forger(ChapterMold).forgeWithSeed(7, { title: "エラーハンドリング" }),
];

const seriesTitle = "Next.js 15 / React 19 実践設計ガイド";
const slug = Forger(SeriesSlugMold).forge({ value: "nextjs-guide" });

export const Default: StoryObj<typeof ChapterPresenter> = {
  args: {
    seriesTitle,
    slug,
    chapterSlug,
    currentChapter: allChapters[0],
    allChapters,
    currentIndex: 0,
    prevChapter: null,
    nextChapter: allChapters[1],
    renderedContent: chapterContent,
  },
};

export const MiddleChapter: StoryObj<typeof ChapterPresenter> = {
  args: {
    seriesTitle,
    slug,
    chapterSlug: allChapters[3].slug,
    currentChapter: allChapters[3],
    allChapters,
    currentIndex: 3,
    prevChapter: allChapters[2],
    nextChapter: allChapters[4],
    renderedContent: chapterContent,
  },
};

export const LastChapter: StoryObj<typeof ChapterPresenter> = {
  args: {
    seriesTitle,
    slug,
    chapterSlug: allChapters[6].slug,
    currentChapter: allChapters[6],
    allChapters,
    currentIndex: 6,
    prevChapter: allChapters[5],
    nextChapter: null,
    renderedContent: chapterContent,
  },
};

export const FewChapters: StoryObj<typeof ChapterPresenter> = {
  args: {
    seriesTitle: "短いシリーズ",
    slug: Forger(SeriesSlugMold).forge({ value: "short-series" }),
    chapterSlug: Forger(ChapterSlugMold).forge({ value: "intro" }),
    currentChapter: Forger(ChapterMold).forge({
      title: "イントロダクション",
      slug: Forger(ChapterSlugMold).forge({ value: "intro" }),
      content: chapterContent,
    }),
    allChapters: [
      Forger(ChapterMold).forge({
        title: "イントロダクション",
        slug: Forger(ChapterSlugMold).forge({ value: "intro" }),
        content: chapterContent,
      }),
      Forger(ChapterMold).forgeWithSeed(2, { title: "まとめ" }),
    ],
    currentIndex: 0,
    prevChapter: null,
    nextChapter: Forger(ChapterMold).forgeWithSeed(2, { title: "まとめ" }),
    renderedContent: chapterContent,
  },
};
