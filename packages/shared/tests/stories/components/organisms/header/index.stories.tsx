import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { HeaderPresenter } from "@shared/components/organisms/header/index.presenter";

const meta = {
  component: HeaderPresenter,
} satisfies Meta<typeof HeaderPresenter>;

export default meta;

export const Default: StoryObj<typeof HeaderPresenter> = {
  args: {
    currentTheme: "light",
    onToggleTheme() {},
    isAdmin: true,
  },
};
