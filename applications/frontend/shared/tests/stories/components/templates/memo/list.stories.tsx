import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoListIndex } from "@shared/components/templates/memo/list";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoMold } from "../../../../support/molds/domains/memo";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";
import { Tag } from "@shared/domains/attributes/tag";

const meta = {
  component: MemoListIndex,
} satisfies Meta<typeof MemoListIndex>;

export default meta;

const tags = Forger(TagMold).forgeMultiWithSeed(5, 1);

const search = async () => Forger(MemoMold).forgeMultiWithSeed(10, 1);

const findAllTags = async (identifiers: string[]): Promise<Tag[]> =>
  tags.filter((tag) => identifiers.includes(tag.identifier));

export const Default: StoryObj<typeof MemoListIndex> = {
  args: {
    search,
    findAllTags,
  },
};

const searchEmpty = async () => [];

export const Empty: StoryObj<typeof MemoListIndex> = {
  args: {
    search: searchEmpty,
    findAllTags,
  },
};

const searchFew = async () => Forger(MemoMold).forgeMultiWithSeed(3, 1);

export const FewItems: StoryObj<typeof MemoListIndex> = {
  args: {
    search: searchFew,
    findAllTags,
  },
};

export const EditLinkMode: StoryObj<typeof MemoListIndex> = {
  args: {
    search,
    findAllTags,
    linkMode: "edit",
  },
};
