import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { DashboardPresenter } from "@shared/components/organisms/dashboard/index.presenter";

const meta = {
  component: DashboardPresenter,
} satisfies Meta<typeof DashboardPresenter>;

export default meta;

export const Default: StoryObj<typeof DashboardPresenter> = {
  args: {
    stats: {
      articles: 24,
      memos: 12,
      drafts: 3,
      monthlyPosts: 5,
    },
    recentItems: [
      {
        identifier: "1",
        title: "Next.js 15の新機能について",
        date: "2024-01-14",
        status: "published",
      },
      {
        identifier: "2",
        title: "TypeScriptの型推論を深掘りする",
        date: "2024-01-13",
        status: "published",
      },
      {
        identifier: "3",
        title: "Reactコンポーネント設計パターン",
        date: "2024-01-12",
        status: "draft",
      },
      {
        identifier: "4",
        title: "CSSモジュールのベストプラクティス",
        date: "2024-01-15",
        status: "scheduled",
      },
    ],
    activities: [
      {
        identifier: "1",
        type: "publish",
        message: "「Next.js 15の新機能について」を公開しました",
        timestamp: "2024-01-14 10:30",
      },
      {
        identifier: "2",
        type: "update",
        message: "「TypeScriptの型推論を深掘りする」を更新しました",
        timestamp: "2024-01-13 15:45",
      },
      {
        identifier: "3",
        type: "create",
        message: "「Reactコンポーネント設計パターン」を作成しました",
        timestamp: "2024-01-12 09:00",
      },
      {
        identifier: "4",
        type: "delete",
        message: "「古い記事」を削除しました",
        timestamp: "2024-01-11 14:20",
      },
    ],
  },
};

export const Empty: StoryObj<typeof DashboardPresenter> = {
  args: {
    stats: {
      articles: 0,
      memos: 0,
      drafts: 0,
      monthlyPosts: 0,
    },
    recentItems: [],
    activities: [],
  },
};

export const HighStats: StoryObj<typeof DashboardPresenter> = {
  args: {
    stats: {
      articles: 150,
      memos: 80,
      drafts: 12,
      monthlyPosts: 25,
    },
    recentItems: [
      {
        identifier: "1",
        title: "大量の記事がある場合のダッシュボード表示",
        date: "2024-01-14",
        status: "published",
      },
    ],
    activities: [
      {
        identifier: "1",
        type: "publish",
        message: "記事を公開しました",
        timestamp: "2024-01-14 10:30",
      },
    ],
  },
};
