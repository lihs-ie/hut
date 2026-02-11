import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { RankingRow } from "@shared/components/atoms/list/ranking";

const meta = {
  component: RankingRow,
  decorators: [
    (Story) => (
      <div style={{ width: "480px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof RankingRow>;

export default meta;

export const Default: StoryObj<typeof RankingRow> = {
  args: {
    rank: 1,
    label: "React入門ガイド",
    value: 320,
    maxValue: 320,
  },
};

export const WithSubLabel: StoryObj<typeof RankingRow> = {
  args: {
    rank: 2,
    label: "Next.js Tips & Tricks",
    value: 210,
    subLabel: "article",
    maxValue: 320,
  },
};

export const LowRank: StoryObj<typeof RankingRow> = {
  args: {
    rank: 10,
    label: "Rust入門",
    value: 45,
    subLabel: "memo",
    maxValue: 320,
  },
};

export const LongLabel: StoryObj<typeof RankingRow> = {
  args: {
    rank: 3,
    label: "TypeScriptの型システムを完全に理解するための実践ガイド - 上級編",
    value: 180,
    maxValue: 320,
  },
};
