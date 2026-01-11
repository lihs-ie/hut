import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TagForm } from "@shared/components/molecules/form/tag";

const meta = {
  component: TagForm,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof TagForm>;
export default meta;

type Story = StoryObj<typeof TagForm>;

export const Empty: Story = {
  args: {
    name: "",
    onNameChange: () => {},
    logoPreview: null,
    onLogoChange: () => {},
  },
};

export const WithName: Story = {
  args: {
    name: "Next.js",
    onNameChange: () => {},
    logoPreview: null,
    onLogoChange: () => {},
  },
};

export const WithLogo: Story = {
  args: {
    name: "React",
    onNameChange: () => {},
    logoPreview: "https://upload.wikimedia.org/wikipedia/commons/a/a7/React-icon.svg",
    onLogoChange: () => {},
  },
};
