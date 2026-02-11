import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ErrorMessage } from "@shared/components/atoms/text/error";

const meta = {
  component: ErrorMessage,
} satisfies Meta<typeof ErrorMessage>;

export default meta;

export const NotFound: StoryObj<typeof ErrorMessage> = {
  args: {
    code: "404",
    title: "ページが見つかりません",
    message: "お探しのページは存在しないか、移動した可能性があります。",
  },
};

export const ServerError: StoryObj<typeof ErrorMessage> = {
  args: {
    code: "500",
    title: "サーバーエラー",
    message: "サーバーで問題が発生しました。しばらくしてからもう一度お試しください。",
  },
};

export const Forbidden: StoryObj<typeof ErrorMessage> = {
  args: {
    code: "403",
    title: "アクセスが拒否されました",
    message: "このページにアクセスする権限がありません。",
  },
};
