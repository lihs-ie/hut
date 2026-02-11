import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { CommonImage } from "@shared/components/atoms/image/common";

const meta = {
  component: CommonImage,
  decorators: [
    (Story) => (
      <div style={{ width: "200px", height: "200px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof CommonImage>;

export default meta;

export const Default: StoryObj<typeof CommonImage> = {
  args: {
    src: "/logo/header-logo.png",
    alt: "サンプル画像",
  },
};
