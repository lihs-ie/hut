import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { CircleImage } from "@shared/components/molecules/image/circle";

const meta = {
  component: CircleImage,
} satisfies Meta<typeof CircleImage>;

export default meta;

export const Default: StoryObj<typeof CircleImage> = {
  args: {
    src: "https://picsum.photos/seed/600/400",
    alt: "Circle Image",
  },
};
