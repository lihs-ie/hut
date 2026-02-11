import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { XLogoImage } from "@shared/components/molecules/image/x-logo";

const meta = {
  component: XLogoImage,
  decorators: [
    (Story) => (
      <div style={{ width: "48px", height: "48px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof XLogoImage>;

export default meta;

export const Default: StoryObj<typeof XLogoImage> = {};
