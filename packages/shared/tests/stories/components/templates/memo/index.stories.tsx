import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoIndex } from "@shared/components/templates/memo";
import { MDXRenderer } from "@shared/components/global/mdx";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoMold } from "../../../../support/molds/domains/memo";

const meta = {
  component: MemoIndex,
} satisfies Meta<typeof MemoIndex>;

export default meta;

export const Default: StoryObj<typeof MemoIndex> = {
  args: {
    find: async (_: string) => Forger(MemoMold).forge(),
    slug: "sample-memo",
    renderer: (content: string) => MDXRenderer(content),
    findAllTags: async () => [],
  },
};
