import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TagCardSkeleton, TagCardSkeletonList } from "@shared/components/molecules/skeleton/tag-card";

const meta = {
  component: TagCardSkeleton,
} satisfies Meta<typeof TagCardSkeleton>;

export default meta;

export const Single: StoryObj<typeof TagCardSkeleton> = {};

export const List: StoryObj = {
  render: () => (
    <div style={{ display: "grid", gap: "1rem" }}>
      <TagCardSkeletonList count={3} />
    </div>
  ),
};

export const GridLayout: StoryObj = {
  render: () => (
    <div style={{ display: "grid", gridTemplateColumns: "repeat(3, 1fr)", gap: "1rem" }}>
      <TagCardSkeletonList count={6} />
    </div>
  ),
};
