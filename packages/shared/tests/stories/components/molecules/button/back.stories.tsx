import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { BackButton } from "@shared/components/molecules/button/back";

const meta = {
  component: BackButton,
} satisfies Meta<typeof BackButton>;

export default meta;

export const Default: StoryObj<typeof BackButton> = {
  args: {
    onClick: () => {},
  },
};

export const Disabled: StoryObj<typeof BackButton> = {
  args: {
    onClick: () => {},
    disabled: true,
  },
};
