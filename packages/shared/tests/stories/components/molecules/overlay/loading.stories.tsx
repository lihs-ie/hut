import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";

const meta = {
  component: LoadingOverlay,
  decorators: [
    (Story) => (
      <div style={{ position: "relative", width: "400px", height: "300px", border: "1px solid #ccc" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof LoadingOverlay>;

export default meta;

export const Default: StoryObj<typeof LoadingOverlay> = {};
