import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesIndex } from "@shared/components/templates/series";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesSlugMold,
  ChapterMold,
  ChapterSlugMold,
} from "../../../../support/molds/domains/series";

const meta = {
  component: SeriesIndex,
} satisfies Meta<typeof SeriesIndex>;

export default meta;

const chapters = [
  Forger(ChapterMold).forge({
    title: "第1章: はじめに",
    slug: Forger(ChapterSlugMold).forge({ value: "chapter-01" }),
    content: "この章では基本的な概念を学びます。",
  }),
  Forger(ChapterMold).forge({
    title: "第2章: 基礎編",
    slug: Forger(ChapterSlugMold).forge({ value: "chapter-02" }),
    content: "基礎的な技術について解説します。",
  }),
  Forger(ChapterMold).forge({
    title: "第3章: 応用編",
    slug: Forger(ChapterSlugMold).forge({ value: "chapter-03" }),
    content: "応用的なテクニックを紹介します。",
  }),
];

const series = Forger(SeriesMold).forge({
  title: "Next.js 15 完全ガイド",
  subTitle: "初心者から上級者まで",
  description:
    "Next.js 15の新機能を網羅的に解説するシリーズです。Server Components、App Router、Server Actionsなど最新の機能を学べます。",
  chapters,
});

const slug = Forger(SeriesSlugMold).forge({ value: "nextjs-15-guide" });

const findBySlug = async () => series;

export const Default: StoryObj<typeof SeriesIndex> = {
  args: {
    slug,
    findBySlug,
  },
};

const seriesWithManyChapters = Forger(SeriesMold).forge({
  title: "React 19 マスターコース",
  subTitle: "Reactの全てを学ぶ",
  description: "React 19の新機能と最新のベストプラクティスを学ぶシリーズです。",
  chapters: Forger(ChapterMold).forgeMultiWithSeed(10, 1),
});

export const ManyChapters: StoryObj<typeof SeriesIndex> = {
  args: {
    slug: Forger(SeriesSlugMold).forge({ value: "react-19-master" }),
    findBySlug: async () => seriesWithManyChapters,
  },
};

const seriesWithSingleChapter = Forger(SeriesMold).forge({
  title: "TypeScript 入門",
  subTitle: null,
  description: "TypeScriptの基礎を1章で学びます。",
  chapters: [
    Forger(ChapterMold).forge({
      title: "TypeScriptとは",
      slug: Forger(ChapterSlugMold).forge({ value: "what-is-typescript" }),
      content: "TypeScriptの基本を説明します。",
    }),
  ],
});

export const SingleChapter: StoryObj<typeof SeriesIndex> = {
  args: {
    slug: Forger(SeriesSlugMold).forge({ value: "typescript-intro" }),
    findBySlug: async () => seriesWithSingleChapter,
  },
};
