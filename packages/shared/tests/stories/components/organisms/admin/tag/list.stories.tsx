import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminTagList } from "../../../../../../../admin/src/app/admin/_components/organisms/admin/tag/list";
import { TagMold } from "../../../../../support/molds/domains/attributes/tag";
import { Forger } from "@lihs-ie/forger-ts";
import { TagName } from "@shared/domains/attributes/tag";

const meta = {
  component: AdminTagList,
  parameters: {
    layout: "fullscreen",
  },
} satisfies Meta<typeof AdminTagList>;
export default meta;

type Story = StoryObj<typeof AdminTagList>;

const tagForger = Forger(TagMold);

export const Default: Story = {
  args: {
    tags: tagForger.forgeMulti(5),
    baseHref: "/tags",
  },
};

export const Empty: Story = {
  args: {
    tags: [],
    baseHref: "/tags",
  },
};

export const ManyTags: Story = {
  args: {
    tags: tagForger.forgeMulti(12),
    baseHref: "/tags",
  },
};

export const WithRealNames: Story = {
  args: {
    tags: [
      tagForger.forge({ name: "TypeScript" as TagName }),
      tagForger.forge({ name: "React" as TagName }),
      tagForger.forge({ name: "Next.js" as TagName }),
      tagForger.forge({ name: "Node.js" as TagName }),
      tagForger.forge({ name: "GraphQL" as TagName }),
    ],
    baseHref: "/tags",
  },
};
