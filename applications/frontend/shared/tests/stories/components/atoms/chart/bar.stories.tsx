import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { BarChart } from "@shared/components/atoms/chart/bar";

const meta = {
  component: BarChart,
  decorators: [
    (Story) => (
      <div style={{ width: "600px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof BarChart>;

export default meta;

export const Default: StoryObj<typeof BarChart> = {
  args: {
    data: [
      { label: "React", value: 85 },
      { label: "Next.js", value: 72 },
      { label: "TypeScript", value: 65 },
      { label: "Rust", value: 40 },
      { label: "Go", value: 35 },
    ],
  },
};

export const SingleItem: StoryObj<typeof BarChart> = {
  args: {
    data: [{ label: "Article", value: 500 }],
  },
};

export const Empty: StoryObj<typeof BarChart> = {
  args: {
    data: [],
  },
};
