import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { ThemeToggle } from "@shared/components/molecules/toggle/theme";
import { ThemeProvider } from "@shared/components/molecules/theme/provider";
import React from "react";

const meta = {
  component: ThemeToggle,
  decorators: [
    (Story) => React.createElement(ThemeProvider, null, React.createElement(Story)),
  ],
} satisfies Meta<typeof ThemeToggle>;

export default meta;

export const Default: StoryObj<typeof ThemeToggle> = {};
