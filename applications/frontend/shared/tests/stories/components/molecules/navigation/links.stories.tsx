import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { NavigationLinks } from "@shared/components/molecules/navigation/links";

const meta = {
  component: NavigationLinks,
} satisfies Meta<typeof NavigationLinks>;

export default meta;

export const Default: StoryObj<typeof NavigationLinks> = {};
