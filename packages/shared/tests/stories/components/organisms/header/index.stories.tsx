import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { HeaderPresenter } from "@shared/components/organisms/header/index.presenter";

const meta = {
  component: HeaderPresenter,
} satisfies Meta<typeof HeaderPresenter>;

export default meta;

export const Default: StoryObj<typeof HeaderPresenter> = {
  args: {
    currentTheme: "light",
    isAdmin: true,
  },
};

export const NotAdmin: StoryObj<typeof HeaderPresenter> = {
  args: {
    currentTheme: "light",
    isAdmin: false,
  },
};

export const DarkTheme: StoryObj<typeof HeaderPresenter> = {
  args: {
    currentTheme: "dark",
    isAdmin: true,
  },
};
