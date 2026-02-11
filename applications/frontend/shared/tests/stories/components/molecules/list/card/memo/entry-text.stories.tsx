import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoEntryTextCard } from "@shared/components/molecules/list/card/memo/entry-text";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoEntryMold } from "../../../../../../support/molds/domains/memo";
import { MDXRenderer } from "@shared/components/global/mdx";

const meta = {
  component: MemoEntryTextCard,
} satisfies Meta<typeof MemoEntryTextCard>;

export default meta;

export const Default: StoryObj<typeof MemoEntryTextCard> = {
  args: {
    entry: Forger(MemoEntryMold).forge(),
    renderer: (content: string) => MDXRenderer(content),
  },
};
