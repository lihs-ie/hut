import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { PieChart } from "@shared/components/atoms/chart/pie";

const meta = {
  component: PieChart,
  decorators: [
    (Story) => (
      <div style={{ width: "400px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof PieChart>;

export default meta;

export const Default: StoryObj<typeof PieChart> = {
  args: {
    data: [
      { label: "Desktop", value: 65 },
      { label: "Mobile", value: 30 },
      { label: "Tablet", value: 5 },
    ],
  },
};

export const TwoSegments: StoryObj<typeof PieChart> = {
  args: {
    data: [
      { label: "Article", value: 70 },
      { label: "Memo", value: 30 },
    ],
  },
};

export const ManySegments: StoryObj<typeof PieChart> = {
  args: {
    data: [
      { label: "Google", value: 45 },
      { label: "Twitter", value: 20 },
      { label: "Direct", value: 15 },
      { label: "GitHub", value: 10 },
      { label: "その他", value: 10 },
    ],
  },
};

export const CustomColors: StoryObj<typeof PieChart> = {
  args: {
    data: [
      { label: "Desktop", value: 65, color: "#6366f1" },
      { label: "Mobile", value: 30, color: "#06b6d4" },
      { label: "Tablet", value: 5, color: "#f59e0b" },
    ],
  },
};
