import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { AnalyticsPeriodSelector } from "@/app/admin/_components/molecules/analytics/period";

const meta = {
  component: AnalyticsPeriodSelector,
} satisfies Meta<typeof AnalyticsPeriodSelector>;

export default meta;

export const Default: StoryObj<typeof AnalyticsPeriodSelector> = {
  args: {
    current: "30d",
  },
};

export const SevenDays: StoryObj<typeof AnalyticsPeriodSelector> = {
  args: {
    current: "7d",
  },
};

export const AllPeriod: StoryObj<typeof AnalyticsPeriodSelector> = {
  args: {
    current: "all",
  },
};
