import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesSummaryCard } from "@shared/components/molecules/list/card/series/summary";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesSlugMold,
} from "../../../../../../support/molds/domains/series";

const meta = {
  component: SeriesSummaryCard,
} satisfies Meta<typeof SeriesSummaryCard>;

export default meta;

const series = Forger(SeriesMold).forge();

export const Default: StoryObj<typeof SeriesSummaryCard> = {
  args: {
    slug: series.slug,
    title: series.title,
    description: series.description,
    cover: series.cover,
    tags: series.tags,
    chapterCount: series.chapters.length,
  },
};

export const WithEmoji: StoryObj<typeof SeriesSummaryCard> = {
  args: {
    slug: Forger(SeriesSlugMold).forge({ value: "nextjs-guide" }),
    title: "Next.js 15 実践ガイド",
    description: "App Router を使った実践的な開発手法を学ぶ",
    cover: null,
    emoji: "📘",
    tags: ["Next.js", "React", "TypeScript"],
    chapterCount: 12,
  },
};

export const NoCover: StoryObj<typeof SeriesSummaryCard> = {
  args: {
    slug: Forger(SeriesSlugMold).forge({ value: "typescript-complete" }),
    title: "TypeScript 完全ガイド",
    description: "基礎から実践まで完全網羅",
    cover: null,
    tags: ["TypeScript", "JavaScript"],
    chapterCount: 15,
  },
};

export const ManyTags: StoryObj<typeof SeriesSummaryCard> = {
  args: {
    slug: Forger(SeriesSlugMold).forge({ value: "web-design" }),
    title: "モダンWebデザイン実践",
    description: "UI/UXデザインの実践テクニック",
    cover: "https://picsum.photos/seed/cover/200/300",
    tags: ["デザイン", "UI/UX", "CSS", "Tailwind", "Figma"],
    chapterCount: 10,
  },
};
