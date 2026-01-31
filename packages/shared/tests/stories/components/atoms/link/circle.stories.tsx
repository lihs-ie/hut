import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { CircleLink } from "@shared/components/atoms/link/circle";
import { GithubImage } from "@shared/components/molecules/image/github";

const meta = {
  component: CircleLink,
} satisfies Meta<typeof CircleLink>;

export default meta;

export const Default: StoryObj<typeof CircleLink> = {
  args: {
    href: "https://example.com",
    children: <GithubImage />,
  },
};
