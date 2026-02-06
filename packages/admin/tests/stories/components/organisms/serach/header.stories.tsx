import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminSearchHeaderPresenter } from "@/app/admin/_components/organisms/search/header.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "@shared/../tests/support/molds/domains/attributes";

const meta = {
  component: AdminSearchHeaderPresenter,
  parameters: {
    layout: "fullscreen",
    nextjs: {
      appDirectory: true,
    },
  },
} satisfies Meta<typeof AdminSearchHeaderPresenter>;

export default meta;

const tags = Forger(TagMold).forgeMulti(20);

export const Default: StoryObj<typeof AdminSearchHeaderPresenter> = {
  args: {
    title: "サンプルコンテンツ",
    tagChoices: tags,
    newContentPath: "#new",
    unvalidated: {},
  },
};
