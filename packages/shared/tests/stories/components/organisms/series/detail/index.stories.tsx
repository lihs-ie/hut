import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesDetail } from "@shared/components/organisms/series/detail";
import { Builder } from "../../../../../support/molds";
import {
  SeriesFactory,
  SeriesSlugFactory,
  ChapterFactory,
} from "../../../../../support/molds/domains/series";
import { Tag } from "@shared/domains/common";

const meta = {
  component: SeriesDetail,
} satisfies Meta<typeof SeriesDetail>;

export default meta;

const series = Builder(SeriesFactory).build({
  title: "Next.js 15 / React 19 実践設計ガイド",
  description:
    "本書では、Next.js 15 / React 19を活用したモダンなWebアプリケーション開発における設計方針を、実装観点ごとに整理しています。App Routerを前提とし、ディレクトリ構成、コンポーネント設計、データ取得、データ更新、状態管理、キャッシュ戦略、エラーハンドリングといった各テーマについて、具体的なユースケースと実装手段を紹介します。",
  tags: [Tag.NEXT_JS, Tag.REACT, Tag.TYPESCRIPT],
  chapters: [
    Builder(ChapterFactory).build({ title: "基本ルールとディレクトリ構成" }),
    Builder(ChapterFactory).buildWith(2, { title: "コンポーネント設計" }),
    Builder(ChapterFactory).buildWith(3, { title: "データ取得" }),
    Builder(ChapterFactory).buildWith(4, { title: "データ更新" }),
    Builder(ChapterFactory).buildWith(5, { title: "状態管理" }),
    Builder(ChapterFactory).buildWith(6, { title: "キャッシュ戦略" }),
    Builder(ChapterFactory).buildWith(7, { title: "エラーハンドリング" }),
  ],
});

const slug = Builder(SeriesSlugFactory).build({ value: "nextjs-guide" });

export const Default: StoryObj<typeof SeriesDetail> = {
  args: {
    series,
    slug,
    author: {
      name: "mori",
      avatar: "https://picsum.photos/seed/mori/100/100",
      bio: "Software Engineer | 最近はClaude Code ×Next.jsでのAI駆動開発を試してます",
    },
  },
};

export const WithoutAuthor: StoryObj<typeof SeriesDetail> = {
  args: {
    series,
    slug,
  },
};

export const WithCover: StoryObj<typeof SeriesDetail> = {
  args: {
    series: Builder(SeriesFactory).build({
      title: "TypeScript 完全ガイド",
      description: "TypeScriptの基礎から高度な型システムまで完全網羅",
      cover: "https://picsum.photos/seed/typescript/400/600",
      tags: [Tag.TYPESCRIPT],
      chapters: Builder(ChapterFactory).buildListWith(5, 10).toArray(),
    }),
    slug: Builder(SeriesSlugFactory).build({ value: "typescript-guide" }),
    author: {
      name: "TypeScript Expert",
      bio: "TypeScript contributor",
    },
  },
};

export const FewChapters: StoryObj<typeof SeriesDetail> = {
  args: {
    series: Builder(SeriesFactory).build({
      title: "短いシリーズ",
      description: "これは短いシリーズです",
      chapters: Builder(ChapterFactory).buildListWith(2, 20).toArray(),
    }),
    slug: Builder(SeriesSlugFactory).build({ value: "short-series" }),
  },
};
