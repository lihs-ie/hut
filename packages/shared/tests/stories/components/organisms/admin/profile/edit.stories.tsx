import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ProfileEditForm } from "../../../../../../../admin/src/app/admin/_components/organisms/admin/profile/edit";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../../support/molds/domains/attributes/tag";
import { UnvalidatedProfile } from "@shared/domains/user";

const meta = {
  component: ProfileEditForm,
  parameters: {
    layout: "padded",
  },
} satisfies Meta<typeof ProfileEditForm>;

export default meta;

type Story = StoryObj<typeof ProfileEditForm>;

const tagForger = Forger(TagMold);

const sampleTags = [
  tagForger.forgeWithSeed(1),
  tagForger.forgeWithSeed(2),
  tagForger.forgeWithSeed(3),
  tagForger.forgeWithSeed(4),
  tagForger.forgeWithSeed(5),
];

const mockPersist = async (unvalidated: UnvalidatedProfile) => {
  console.log("Persisting profile:", unvalidated);
  await new Promise((resolve) => setTimeout(resolve, 1000));
};

export const CreateNew: Story = {
  args: {
    initial: undefined,
    tags: sampleTags,
    persist: mockPersist,
  },
};

export const WithManyTags: Story = {
  args: {
    initial: undefined,
    tags: Array.from({ length: 15 }, (_, index) => tagForger.forgeWithSeed(index + 1)),
    persist: mockPersist,
  },
};

export const NoTags: Story = {
  args: {
    initial: undefined,
    tags: [],
    persist: mockPersist,
  },
};
