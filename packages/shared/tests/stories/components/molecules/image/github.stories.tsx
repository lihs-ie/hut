import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { GithubImage } from "@shared/components/molecules/image/github";

const meta = {
  component: GithubImage,
  decorators: [
    (Story) => (
      <div style={{ width: "48px", height: "48px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof GithubImage>;

export default meta;

export const Default: StoryObj<typeof GithubImage> = {};
