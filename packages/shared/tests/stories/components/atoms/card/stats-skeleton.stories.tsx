import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { StatsCardSkeleton } from "@shared/components/atoms/card/stats.skeleton";

const meta = {
  component: StatsCardSkeleton,
  decorators: [
    (Story) => (
      <div style={{ display: "grid", gridTemplateColumns: "repeat(4, 1fr)", gap: "1rem", width: "800px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof StatsCardSkeleton>;

export default meta;

export const Default: StoryObj<typeof StatsCardSkeleton> = {
  args: {},
};

export const TwoCards: StoryObj<typeof StatsCardSkeleton> = {
  args: {
    count: 2,
  },
};
