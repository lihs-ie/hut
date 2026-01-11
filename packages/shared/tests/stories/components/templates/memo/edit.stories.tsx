import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoEditIndex } from "@shared/components/templates/memo/edit";
import { useState } from "react";
import { addEntry, MemoSlug, validateEntry } from "@shared/domains/memo";
import { MDXRenderer } from "@shared/components/global/mdx";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoMold } from "../../../../support/molds/domains/memo";
import { slugSchema } from "@shared/domains/common";

const meta = {
  component: MemoEditIndex,
} satisfies Meta<typeof MemoEditIndex>;

export default meta;

const initial = Forger(MemoMold).forge();

export const Default: StoryObj<typeof MemoEditIndex> = {
  render: () => {
    const [memo, setMemo] = useState(initial);

    const getEntries = async (_: MemoSlug) => {
      return memo.entries;
    };

    return (
      <meta.component
        getEntries={getEntries}
        renderer={(content) => MDXRenderer(content)}
        addEntry={async (_, text) => {
          setMemo(
            addEntry(
              memo,
              validateEntry({ text, createdAt: new Date() }).unwrap()
            )
          );
        }}
        slug={slugSchema.parse("sample")}
        find={async () => memo}
        sidebar={{
          closeMemo: async () => {},
          updateStatus: async () => {},
          slug: slugSchema.parse("sample"),
        }}
      />
    );
  },
};
