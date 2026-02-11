import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ContentCardSkeleton, ContentCardSkeletonList } from "@shared/components/molecules/skeleton/content-card";

const meta = {
  component: ContentCardSkeleton,
} satisfies Meta<typeof ContentCardSkeleton>;

export default meta;

export const Single: StoryObj<typeof ContentCardSkeleton> = {};

export const List: StoryObj = {
  render: () => (
    <div style={{ display: "grid", gap: "1rem" }}>
      <ContentCardSkeletonList count={3} />
    </div>
  ),
};

export const GridLayout: StoryObj = {
  render: () => (
    <div style={{ display: "grid", gridTemplateColumns: "repeat(2, 1fr)", gap: "1rem" }}>
      <ContentCardSkeletonList count={6} />
    </div>
  ),
};
