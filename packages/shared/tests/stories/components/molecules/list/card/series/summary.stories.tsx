import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesSummaryCard } from "@shared/components/molecules/list/card/series/summary";
import { Builder } from "../../../../../../support/molds";
import {
  SeriesFactory,
  SeriesSlugFactory,
  SeriesTitleFactory,
  SeriesDescriptionFactory,
} from "../../../../../../support/molds/domains/series";

const meta = {
  component: SeriesSummaryCard,
} satisfies Meta<typeof SeriesSummaryCard>;

export default meta;

const series = Builder(SeriesFactory).build();

export const Default: StoryObj<typeof SeriesSummaryCard> = {
  args: {
    slug: series.slug,
    title: series.title,
    description: series.description,
    cover: series.cover,
    tags: series.tags,
    chapterCount: series.chapters.length,
    author: {
      name: "Author Name",
      avatar: "https://picsum.photos/seed/author/100/100",
    },
  },
};

export const WithEmoji: StoryObj<typeof SeriesSummaryCard> = {
  args: {
    slug: Builder(SeriesSlugFactory).build({ value: "nextjs-guide" }),
    title: "Next.js 15 実践ガイド",
    description: "App Router を使った実践的な開発手法を学ぶ",
    cover: null,
    emoji: "📘",
    tags: ["Next.js", "React", "TypeScript"],
    chapterCount: 12,
    author: {
      name: "Author Name",
      avatar: "https://picsum.photos/seed/author2/100/100",
    },
  },
};

export const NoCover: StoryObj<typeof SeriesSummaryCard> = {
  args: {
    slug: Builder(SeriesSlugFactory).build({ value: "typescript-complete" }),
    title: "TypeScript 完全ガイド",
    description: "基礎から実践まで完全網羅",
    cover: null,
    tags: ["TypeScript", "JavaScript"],
    chapterCount: 15,
  },
};

export const ManyTags: StoryObj<typeof SeriesSummaryCard> = {
  args: {
    slug: Builder(SeriesSlugFactory).build({ value: "web-design" }),
    title: "モダンWebデザイン実践",
    description: "UI/UXデザインの実践テクニック",
    cover: "https://picsum.photos/seed/cover/200/300",
    tags: ["デザイン", "UI/UX", "CSS", "Tailwind", "Figma"],
    chapterCount: 10,
    author: {
      name: "Designer",
    },
  },
};
