import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { LineChart } from "@shared/components/atoms/chart/line";

const meta = {
  component: LineChart,
  decorators: [
    (Story) => (
      <div style={{ width: "600px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof LineChart>;

export default meta;

const dailyData = Array.from({ length: 30 }, (_, index) => ({
  label: `1/${index + 1}`,
  value: Math.floor(Math.random() * 200) + 50,
}));

export const Default: StoryObj<typeof LineChart> = {
  args: {
    data: dailyData,
  },
};

export const CustomColor: StoryObj<typeof LineChart> = {
  args: {
    data: dailyData,
    color: "var(--accent)",
  },
};

export const CustomHeight: StoryObj<typeof LineChart> = {
  args: {
    data: dailyData,
    height: 200,
  },
};

export const FewDataPoints: StoryObj<typeof LineChart> = {
  args: {
    data: [
      { label: "1月", value: 120 },
      { label: "2月", value: 180 },
      { label: "3月", value: 150 },
      { label: "4月", value: 220 },
    ],
  },
};

export const Empty: StoryObj<typeof LineChart> = {
  args: {
    data: [],
  },
};
