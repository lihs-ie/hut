import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesDetail } from "@shared/components/organisms/series/detail";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesSlugMold,
  ChapterMold,
} from "../../../../../support/molds/domains/series";
import { TagIdentifierMold } from "../../../../../support/molds/domains/attributes/tag";

const meta = {
  component: SeriesDetail,
} satisfies Meta<typeof SeriesDetail>;

export default meta;

const seriesForger = Forger(SeriesMold);
const slugForger = Forger(SeriesSlugMold);
const chapterForger = Forger(ChapterMold);
const tagIdentifierForger = Forger(TagIdentifierMold);

const series = seriesForger.forge({
  title: "Next.js 15 / React 19 実践設計ガイド",
  description:
    "本書では、Next.js 15 / React 19を活用したモダンなWebアプリケーション開発における設計方針を、実装観点ごとに整理しています。App Routerを前提とし、ディレクトリ構成、コンポーネント設計、データ取得、データ更新、状態管理、キャッシュ戦略、エラーハンドリングといった各テーマについて、具体的なユースケースと実装手段を紹介します。",
  tags: tagIdentifierForger.forgeMulti(3),
  chapters: [
    chapterForger.forge({ title: "基本ルールとディレクトリ構成" }),
    chapterForger.forgeWithSeed(2, { title: "コンポーネント設計" }),
    chapterForger.forgeWithSeed(3, { title: "データ取得" }),
    chapterForger.forgeWithSeed(4, { title: "データ更新" }),
    chapterForger.forgeWithSeed(5, { title: "状態管理" }),
    chapterForger.forgeWithSeed(6, { title: "キャッシュ戦略" }),
    chapterForger.forgeWithSeed(7, { title: "エラーハンドリング" }),
  ],
});

const slug = slugForger.forge();

export const Default: StoryObj<typeof SeriesDetail> = {
  args: {
    series,
    slug,
  },
};

export const WithCover: StoryObj<typeof SeriesDetail> = {
  args: {
    series: seriesForger.forge({
      title: "TypeScript 完全ガイド",
      description: "TypeScriptの基礎から高度な型システムまで完全網羅",
      cover: "https://picsum.photos/seed/typescript/400/600",
      tags: tagIdentifierForger.forgeMulti(1),
      chapters: chapterForger.forgeMultiWithSeed(5, 10),
    }),
    slug: slugForger.forge(),
  },
};

export const FewChapters: StoryObj<typeof SeriesDetail> = {
  args: {
    series: seriesForger.forge({
      title: "短いシリーズ",
      description: "これは短いシリーズです",
      chapters: chapterForger.forgeMultiWithSeed(2, 20),
    }),
    slug: slugForger.forge(),
  },
};
