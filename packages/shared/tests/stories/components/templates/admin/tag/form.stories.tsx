import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminTagFormTemplate } from "@shared/components/templates/admin/tag";
import { TagMold } from "../../../../../support/molds/domains/attributes/tag";
import { Forger } from "@lihs-ie/forger-ts";
import { tagNameSchema } from "@shared/domains/attributes/tag";
import { imageSchema } from "@shared/domains/common/image";

const meta = {
  component: AdminTagFormTemplate,
  parameters: {
    layout: "fullscreen",
  },
} satisfies Meta<typeof AdminTagFormTemplate>;
export default meta;

type Story = StoryObj<typeof AdminTagFormTemplate>;

export const CreateNew: Story = {
  args: {
    onSubmit: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    backHref: "/tags",
    cancelHref: "/tags",
  },
};

export const EditExisting: Story = {
  args: {
    tag: Forger(TagMold).forge(),
    onSubmit: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    onDelete: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    backHref: "/tags",
    cancelHref: "/tags",
  },
};

export const EditWithLogo: Story = {
  args: {
    tag: Forger(TagMold).forge({
      name: tagNameSchema.parse("TypeScript"),
      logo: imageSchema.parse(
        "https://upload.wikimedia.org/wikipedia/commons/4/4c/Typescript_logo_2020.svg"
      ),
    }),
    onSubmit: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    onDelete: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    backHref: "/tags",
    cancelHref: "/tags",
  },
};
