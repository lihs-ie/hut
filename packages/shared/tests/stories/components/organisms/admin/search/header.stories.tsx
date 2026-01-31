import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminSearchHeaderPresenter } from "../../../../../../../admin/src/app/admin/_components/organisms/search/header.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../../support/molds/domains/attributes/tag";

const meta = {
  component: AdminSearchHeaderPresenter,
  parameters: {
    layout: "padded",
    nextjs: {
      appDirectory: true,
    },
  },
} satisfies Meta<typeof AdminSearchHeaderPresenter>;

export default meta;

type Story = StoryObj<typeof AdminSearchHeaderPresenter>;

const tagForger = Forger(TagMold);

const sampleTags = [
  tagForger.forgeWithSeed(1),
  tagForger.forgeWithSeed(2),
  tagForger.forgeWithSeed(3),
  tagForger.forgeWithSeed(4),
  tagForger.forgeWithSeed(5),
];

export const ArticleSearch: Story = {
  args: {
    title: "記事の管理",
    newContentPath: "/admin/articles/new",
    tagChoices: sampleTags,
    unvalidated: {},
  },
};

export const MemoSearch: Story = {
  args: {
    title: "メモの管理",
    newContentPath: "/admin/memos/new",
    tagChoices: sampleTags,
    unvalidated: {},
  },
};

export const SeriesSearch: Story = {
  args: {
    title: "シリーズの管理",
    newContentPath: "/admin/series/new",
    tagChoices: sampleTags,
    unvalidated: {},
  },
};

export const WithManyTags: Story = {
  args: {
    title: "コンテンツ管理",
    newContentPath: "/admin/contents/new",
    tagChoices: Array.from({ length: 20 }, (_, index) =>
      tagForger.forgeWithSeed(index + 1)
    ),
    unvalidated: {},
  },
};

export const NoTags: Story = {
  args: {
    title: "タグなしの検索",
    newContentPath: "/admin/articles/new",
    tagChoices: [],
    unvalidated: {},
  },
};
