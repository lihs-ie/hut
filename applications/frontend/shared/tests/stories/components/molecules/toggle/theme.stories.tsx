import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ThemeToggle } from "@shared/components/molecules/toggle/theme";

const meta = {
  component: ThemeToggle,
} satisfies Meta<typeof ThemeToggle>;

export default meta;

export const Light: StoryObj<typeof ThemeToggle> = {
  args: {
    value: "light",
  },
};

export const Dark: StoryObj<typeof ThemeToggle> = {
  args: {
    value: "dark",
  },
};
