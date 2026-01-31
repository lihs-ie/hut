import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { StatsCard } from "@shared/components/atoms/card/stats";

const meta = {
  component: StatsCard,
} satisfies Meta<typeof StatsCard>;

export default meta;

export const Default: StoryObj<typeof StatsCard> = {
  args: {
    title: "記事数",
    value: 24,
  },
};

export const WithTrendUp: StoryObj<typeof StatsCard> = {
  args: {
    title: "記事数",
    value: 24,
    trend: {
      value: 12,
      label: "先月比",
    },
  },
};

export const WithTrendDown: StoryObj<typeof StatsCard> = {
  args: {
    title: "今月の投稿",
    value: 5,
    trend: {
      value: -8,
      label: "先月比",
    },
  },
};

export const WithTrendNeutral: StoryObj<typeof StatsCard> = {
  args: {
    title: "下書き",
    value: 3,
    trend: {
      value: 0,
      label: "先月比",
    },
  },
};

export const StringValue: StoryObj<typeof StatsCard> = {
  args: {
    title: "ステータス",
    value: "公開中",
  },
};
