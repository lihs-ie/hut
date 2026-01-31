import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ErrorIndex } from "@shared/components/templates/error";

const meta = {
  component: ErrorIndex,
  parameters: {
    nextjs: { appDirectory: true },
  },
} satisfies Meta<typeof ErrorIndex>;

export default meta;

export const NotFound: StoryObj<typeof ErrorIndex> = {
  args: {
    code: "404",
    title: "ページが見つかりません",
    message: "お探しのページは存在しないか、移動した可能性があります。",
    showHomeButton: true,
    showBackButton: true,
  },
};

export const ServerError: StoryObj<typeof ErrorIndex> = {
  args: {
    code: "500",
    title: "サーバーエラー",
    message: "サーバーで問題が発生しました。しばらくしてからもう一度お試しください。",
    showHomeButton: true,
    showBackButton: false,
  },
};

export const Forbidden: StoryObj<typeof ErrorIndex> = {
  args: {
    code: "403",
    title: "アクセスが拒否されました",
    message: "このページにアクセスする権限がありません。",
    showHomeButton: true,
    showBackButton: true,
  },
};

export const WithRetryHandler: StoryObj<typeof ErrorIndex> = {
  args: {
    code: "500",
    title: "接続エラー",
    message: "ネットワーク接続に問題が発生しました。",
    showHomeButton: false,
    showBackButton: false,
    onRetry: () => {
      console.log("Retry clicked");
    },
  },
};

export const MinimalError: StoryObj<typeof ErrorIndex> = {
  args: {
    code: "400",
    title: "リクエストエラー",
    message: "不正なリクエストです。",
    showHomeButton: false,
    showBackButton: false,
  },
};
