import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TextLink } from "@/components/atoms/link/text";

const meta = {
  component: TextLink,
} satisfies Meta<typeof TextLink>;
export default meta;

export const Default: StoryObj<typeof TextLink> = {
  args: {
    href: "https://example.com",
    children: "Sample",
  },
};
