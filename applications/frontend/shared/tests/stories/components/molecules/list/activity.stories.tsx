import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ActivityList } from "@shared/components/molecules/list/activity";

const meta = {
  component: ActivityList,
} satisfies Meta<typeof ActivityList>;

export default meta;

export const Default: StoryObj<typeof ActivityList> = {
  args: {
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

export const CreateOnly: StoryObj<typeof ActivityList> = {
  args: {
    activities: [
      {
        identifier: "1",
        type: "create",
        message: "新しい記事を作成しました",
        timestamp: "2024-01-14 10:30",
      },
      {
        identifier: "2",
        type: "create",
        message: "メモを作成しました",
        timestamp: "2024-01-13 15:45",
      },
    ],
  },
};

export const UpdateOnly: StoryObj<typeof ActivityList> = {
  args: {
    activities: [
      {
        identifier: "1",
        type: "update",
        message: "記事を更新しました",
        timestamp: "2024-01-14 10:30",
      },
      {
        identifier: "2",
        type: "update",
        message: "メモを更新しました",
        timestamp: "2024-01-13 15:45",
      },
    ],
  },
};

export const SingleItem: StoryObj<typeof ActivityList> = {
  args: {
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

export const Empty: StoryObj<typeof ActivityList> = {
  args: {
    activities: [],
  },
};
