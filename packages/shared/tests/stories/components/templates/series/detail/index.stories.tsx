import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesDetailIndex } from "@shared/components/templates/series/detail";
import { Builder } from "../../../../../support/molds";
import {
  SeriesFactory,
  SeriesSlugFactory,
  ChapterFactory,
} from "../../../../../support/molds/domains/series";
import { Tag } from "@shared/domains/common";

const meta = {
  component: SeriesDetailIndex,
} satisfies Meta<typeof SeriesDetailIndex>;

export default meta;

const series = Builder(SeriesFactory).build({
  title: "Next.js 15 / React 19 実践設計ガイド",
  description:
    "本書では、Next.js 15 / React 19を活用したモダンなWebアプリケーション開発における設計方針を、実装観点ごとに整理しています。",
  tags: [Tag.NEXT_JS, Tag.REACT, Tag.TYPESCRIPT],
  chapters: [
    Builder(ChapterFactory).build({ title: "基本ルールとディレクトリ構成" }),
    Builder(ChapterFactory).buildWith(2, { title: "コンポーネント設計" }),
    Builder(ChapterFactory).buildWith(3, { title: "データ取得" }),
    Builder(ChapterFactory).buildWith(4, { title: "データ更新" }),
    Builder(ChapterFactory).buildWith(5, { title: "状態管理" }),
  ],
});

const slug = Builder(SeriesSlugFactory).build({ value: "nextjs-guide" });

export const Default: StoryObj<typeof SeriesDetailIndex> = {
  args: {
    series,
    slug,
    author: {
      name: "mori",
      avatar: "https://picsum.photos/seed/mori/100/100",
      bio: "Software Engineer",
    },
  },
};

export const WithoutAuthor: StoryObj<typeof SeriesDetailIndex> = {
  args: {
    series,
    slug,
  },
};
