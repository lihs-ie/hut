import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { RemoveButton } from "@shared/components/molecules/button/remove";

const meta = {
  component: RemoveButton,
} satisfies Meta<typeof RemoveButton>;

export default meta;

export const Default: StoryObj<typeof RemoveButton> = {
  args: {
    onClick: () => {},
  },
};

export const WithAriaLabel: StoryObj<typeof RemoveButton> = {
  args: {
    onClick: () => {},
    ariaLabel: "タグを削除",
  },
};
