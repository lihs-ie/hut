import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PlayButton } from "@/app/admin/_components/molecules/button/play";

const meta = {
  component: PlayButton,
} satisfies Meta<typeof PlayButton>;

export default meta;

export const Default: StoryObj<typeof PlayButton> = {
  args: {
    href: "#",
  },
};
