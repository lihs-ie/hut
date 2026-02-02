import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoEntriesPresenter } from "@shared/components/organisms/memo/edit/list.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoEntryMold } from "../../../../../support/molds/domains/memo";
import { MDXRenderer } from "@shared/components/global/mdx";

const meta = {
  component: MemoEntriesPresenter,
} satisfies Meta<typeof MemoEntriesPresenter>;
export default meta;

export const Default: StoryObj<typeof MemoEntriesPresenter> = {
  args: {
    entries: Forger(MemoEntryMold).forgeMultiWithSeed(5, 1),
    renderer: (content) => MDXRenderer(content),
  },
};

export const Empty: StoryObj<typeof MemoEntriesPresenter> = {
  args: {
    entries: [],
    renderer: (content) => MDXRenderer(content),
  },
};
