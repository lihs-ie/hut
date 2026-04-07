import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { HeaderPresenter } from "@shared/components/organisms/header/index.presenter";
import { ThemeProvider } from "@shared/components/molecules/theme/provider";
import React from "react";

const meta = {
  component: HeaderPresenter,
  decorators: [
    (Story) => React.createElement(ThemeProvider, null, React.createElement(Story)),
  ],
} satisfies Meta<typeof HeaderPresenter>;

export default meta;

export const Default: StoryObj<typeof HeaderPresenter> = {
  args: {
    isAdmin: true,
  },
};

export const NotAdmin: StoryObj<typeof HeaderPresenter> = {
  args: {
    isAdmin: false,
  },
};
