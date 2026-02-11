import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoTitleCard } from "@shared/components/molecules/list/card/memo/title";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoMold } from "../../../../../../support/molds/domains/memo";

const meta = {
  component: MemoTitleCard,
} satisfies Meta<typeof MemoTitleCard>;

export default meta;

const memo = Forger(MemoMold).forge();

export const Default: StoryObj<typeof MemoTitleCard> = {
  args: {
    title: memo.title,
    timeline: memo.timeline,
    slug: memo.slug,
  },
};
