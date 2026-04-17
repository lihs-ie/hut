import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ChapterContentPresenter } from "@shared/components/organisms/series/chapter/content.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  ChapterSlugMold,
  SeriesSlugMold,
} from "../../../../../support/molds/domains/series";

const meta = {
  component: ChapterContentPresenter,
} satisfies Meta<typeof ChapterContentPresenter>;

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
└── layout.tsx
\`\`\`
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
];

const slug = Forger(SeriesSlugMold).forge({ value: "nextjs-guide" });

export const Default: StoryObj<typeof ChapterContentPresenter> = {
  args: {
    slug,
    chapterSlug,
    currentChapter: allChapters[0],
    currentIndex: 0,
    prevChapter: null,
    nextChapter: allChapters[1],
    renderedContent: chapterContent,
  },
};

export const MiddleChapter: StoryObj<typeof ChapterContentPresenter> = {
  args: {
    slug,
    chapterSlug: allChapters[1].slug,
    currentChapter: allChapters[1],
    currentIndex: 1,
    prevChapter: allChapters[0],
    nextChapter: allChapters[2],
    renderedContent: chapterContent,
  },
};

export const LastChapter: StoryObj<typeof ChapterContentPresenter> = {
  args: {
    slug,
    chapterSlug: allChapters[2].slug,
    currentChapter: allChapters[2],
    currentIndex: 2,
    prevChapter: allChapters[1],
    nextChapter: null,
    renderedContent: chapterContent,
  },
};
