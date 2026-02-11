import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { HeaderLogo } from "@shared/components/atoms/logo/header";

const meta = {
  component: HeaderLogo,
  decorators: [
    (Story) => (
      <div style={{ width: "100px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof HeaderLogo>;

export default meta;

export const Default: StoryObj<typeof HeaderLogo> = {};
