import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { RecentList } from "@shared/components/molecules/list/recent";

const meta = {
  component: RecentList,
} satisfies Meta<typeof RecentList>;

export default meta;

export const Default: StoryObj<typeof RecentList> = {
  args: {
    items: [
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
  },
};

export const AllPublished: StoryObj<typeof RecentList> = {
  args: {
    items: [
      {
        identifier: "1",
        title: "公開済み記事1",
        date: "2024-01-14",
        status: "published",
      },
      {
        identifier: "2",
        title: "公開済み記事2",
        date: "2024-01-13",
        status: "published",
      },
    ],
  },
};

export const AllDrafts: StoryObj<typeof RecentList> = {
  args: {
    items: [
      {
        identifier: "1",
        title: "下書き記事1",
        date: "2024-01-14",
        status: "draft",
      },
      {
        identifier: "2",
        title: "下書き記事2",
        date: "2024-01-13",
        status: "draft",
      },
    ],
  },
};

export const Empty: StoryObj<typeof RecentList> = {
  args: {
    items: [],
  },
};

export const LongTitle: StoryObj<typeof RecentList> = {
  args: {
    items: [
      {
        identifier: "1",
        title:
          "これは非常に長いタイトルの記事です。タイトルが長すぎる場合にどのように表示されるかをテストしています。",
        date: "2024-01-14",
        status: "published",
      },
    ],
  },
};
