import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TitlePresenter } from "@shared/components/organisms/common/title.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import { TimelineMold } from "../../../../support/molds/domains/common/date";

const meta = {
  component: TitlePresenter,
} satisfies Meta<typeof TitlePresenter>;

export default meta;

export const Default: StoryObj<typeof TitlePresenter> = {
  args: {
    title: "サンプル記事タイトル",
    timeline: Forger(TimelineMold).forge(),
  },
};

export const LongTitle: StoryObj<typeof TitlePresenter> = {
  args: {
    title:
      "これは非常に長いタイトルで、タイトルコンポーネントのレイアウトがどのように処理されるかをテストするためのサンプルです",
    timeline: Forger(TimelineMold).forge(),
  },
};

export const ShortTitle: StoryObj<typeof TitlePresenter> = {
  args: {
    title: "短いタイトル",
    timeline: Forger(TimelineMold).forge(),
  },
};

export const RecentlyUpdated: StoryObj<typeof TitlePresenter> = {
  args: {
    title: "最近更新された記事",
    timeline: {
      createdAt: new Date("2024-01-01"),
      updatedAt: new Date(),
    },
  },
};

export const OldArticle: StoryObj<typeof TitlePresenter> = {
  args: {
    title: "古い記事",
    timeline: {
      createdAt: new Date("2020-01-15"),
      updatedAt: new Date("2020-02-20"),
    },
  },
};
