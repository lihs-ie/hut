import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { LogoutButton } from "@shared/components/molecules/button/logout";

const meta = {
  component: LogoutButton,
} satisfies Meta<typeof LogoutButton>;

export default meta;

export const Default: StoryObj<typeof LogoutButton> = {
  args: {
    logout: async () => {},
  },
};
