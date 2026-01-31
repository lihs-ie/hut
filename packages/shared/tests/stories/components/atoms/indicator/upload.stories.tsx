import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { UploadIndicator } from "@shared/components/atoms/indicator/upload";

const meta = {
  component: UploadIndicator,
  decorators: [
    (Story) => (
      <div style={{ width: "300px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof UploadIndicator>;

export default meta;

export const Compressing: StoryObj<typeof UploadIndicator> = {
  args: {
    progress: 30,
    status: "compressing",
  },
};

export const Uploading: StoryObj<typeof UploadIndicator> = {
  args: {
    progress: 60,
    status: "uploading",
  },
};

export const Completed: StoryObj<typeof UploadIndicator> = {
  args: {
    progress: 100,
    status: "completed",
  },
};

export const Failed: StoryObj<typeof UploadIndicator> = {
  args: {
    progress: 45,
    status: "failed",
  },
};
