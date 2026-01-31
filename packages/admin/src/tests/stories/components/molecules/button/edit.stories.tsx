import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { EditButton } from "@/app/admin/_components/molecules/button/edit";

const meta = {
  component: EditButton,
} satisfies Meta<typeof EditButton>;
export default meta;

export const Default: StoryObj<typeof EditButton> = {
  args: {
    href: "#",
  },
};
