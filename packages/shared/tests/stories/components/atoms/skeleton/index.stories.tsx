import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Skeleton, SkeletonText, SkeletonCircle } from "@shared/components/atoms/skeleton";

const meta = {
  component: Skeleton,
} satisfies Meta<typeof Skeleton>;

export default meta;

export const Default: StoryObj<typeof Skeleton> = {
  args: {
    width: "200px",
    height: "20px",
  },
};

export const TextVariant: StoryObj<typeof Skeleton> = {
  args: {
    width: "150px",
    height: "1rem",
    variant: "text",
  },
};

export const CircleVariant: StoryObj<typeof Skeleton> = {
  args: {
    width: "60px",
    height: "60px",
    variant: "circle",
  },
};

export const CardVariant: StoryObj<typeof Skeleton> = {
  args: {
    width: "300px",
    height: "150px",
    variant: "card",
  },
};

export const RectangleVariant: StoryObj<typeof Skeleton> = {
  args: {
    width: "200px",
    height: "100px",
    variant: "rectangle",
  },
};

export const SkeletonTextComponent: StoryObj = {
  render: () => (
    <div style={{ width: "300px" }}>
      <SkeletonText lines={3} />
    </div>
  ),
};

export const SkeletonCircleComponent: StoryObj = {
  render: () => <SkeletonCircle size="80px" />,
};
