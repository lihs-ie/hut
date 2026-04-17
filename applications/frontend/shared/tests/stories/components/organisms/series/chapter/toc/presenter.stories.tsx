import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ChapterTOCPresenter } from "@shared/components/organisms/series/chapter/toc/presenter";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  ChapterSlugMold,
  SeriesSlugMold,
} from "../../../../../../support/molds/domains/series";

const meta = {
  component: ChapterTOCPresenter,
} satisfies Meta<typeof ChapterTOCPresenter>;

export default meta;

const chapters = [
  Forger(ChapterMold).forge({
    title: "基本ルールとディレクトリ構成",
    slug: Forger(ChapterSlugMold).forge({ value: "chapter-01" }),
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

export const Default: StoryObj<typeof ChapterTOCPresenter> = {
  args: {
    seriesTitle,
    slug,
    chapters,
  },
};

export const FewChapters: StoryObj<typeof ChapterTOCPresenter> = {
  args: {
    seriesTitle: "短いシリーズ",
    slug: Forger(SeriesSlugMold).forge({ value: "short-series" }),
    chapters: [
      Forger(ChapterMold).forge({ title: "イントロダクション" }),
      Forger(ChapterMold).forgeWithSeed(2, { title: "まとめ" }),
    ],
  },
};
