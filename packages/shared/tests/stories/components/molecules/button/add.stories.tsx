import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AddButton } from "@shared/components/molecules/button/add";

const meta = {
  component: AddButton,
} satisfies Meta<typeof AddButton>;

export default meta;

export const Default: StoryObj<typeof AddButton> = {
  args: {
    hasLabel: true,
    onClick: () => {},
  },
};

export const WithoutLabel: StoryObj<typeof AddButton> = {
  args: {
    hasLabel: false,
    onClick: () => {},
  },
};
