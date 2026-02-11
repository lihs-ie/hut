import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { ChartSkeleton } from "@shared/components/atoms/chart/skeleton";

const meta = {
  component: ChartSkeleton,
  decorators: [
    (Story) => (
      <div style={{ width: "600px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof ChartSkeleton>;

export default meta;

export const Default: StoryObj<typeof ChartSkeleton> = {
  args: {},
};

export const Tall: StoryObj<typeof ChartSkeleton> = {
  args: {
    height: 400,
  },
};

export const Short: StoryObj<typeof ChartSkeleton> = {
  args: {
    height: 150,
  },
};
