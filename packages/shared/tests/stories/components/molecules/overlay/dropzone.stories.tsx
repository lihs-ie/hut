import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { DropzoneOverlay } from "@shared/components/molecules/overlay/dropzone";

const meta = {
  component: DropzoneOverlay,
  decorators: [
    (Story) => (
      <div style={{ position: "relative", width: "400px", height: "300px", border: "1px solid #ccc" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof DropzoneOverlay>;

export default meta;

export const Active: StoryObj<typeof DropzoneOverlay> = {
  args: {
    isActive: true,
  },
};

export const Inactive: StoryObj<typeof DropzoneOverlay> = {
  args: {
    isActive: false,
  },
};
