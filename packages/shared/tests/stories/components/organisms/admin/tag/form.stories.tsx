import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminTagForm } from "../../../../../../../admin/src/app/admin/_components/organisms/admin/tag/form";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../../support/molds/domains/attributes/tag";
import { TagName } from "@shared/domains/attributes/tag";
import { Image } from "@shared/domains/common/image";

const meta = {
  component: AdminTagForm,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof AdminTagForm>;
export default meta;

type Story = StoryObj<typeof AdminTagForm>;

export const CreateNew: Story = {
  args: {
    isNew: true,
    onSubmit: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    cancelHref: "/tags",
  },
};

export const EditExisting: Story = {
  args: {
    initialTag: Forger(TagMold).forge({ name: "JavaScript" as TagName }),
    isNew: false,
    onSubmit: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    onDelete: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    cancelHref: "/tags",
  },
};

export const EditWithLogo: Story = {
  args: {
    initialTag: Forger(TagMold).forge({
      name: "TypeScript" as TagName,
      logo: "https://upload.wikimedia.org/wikipedia/commons/4/4c/Typescript_logo_2020.svg" as Image,
    }),
    isNew: false,
    onSubmit: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    onDelete: async () => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    },
    cancelHref: "/tags",
  },
};
