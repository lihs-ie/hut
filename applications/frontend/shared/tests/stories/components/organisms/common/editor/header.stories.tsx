import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { EditorHeader } from "@shared/components/organisms/common/editor/header";

const meta = {
  component: EditorHeader,
  parameters: {
    nextjs: { appDirectory: true },
  },
  args: {
    onTitleChange: (title: string) => console.log("onTitleChange:", title),
    onPublishChange: (value: boolean) =>
      console.log("onPublishChange:", value),
    persist: async () => console.log("persist called"),
  },
} satisfies Meta<typeof EditorHeader>;

export default meta;

export const Default: StoryObj<typeof EditorHeader> = {
  args: {
    title: "記事タイトル",
    isPublished: false,
    isLoading: false,
  },
};

export const Published: StoryObj<typeof EditorHeader> = {
  args: {
    title: "公開済み記事",
    isPublished: true,
    isLoading: false,
  },
};

export const Loading: StoryObj<typeof EditorHeader> = {
  args: {
    title: "保存中の記事",
    isPublished: false,
    isLoading: true,
  },
};

export const EmptyTitle: StoryObj<typeof EditorHeader> = {
  args: {
    title: "",
    isPublished: false,
    isLoading: false,
  },
};

export const LongTitle: StoryObj<typeof EditorHeader> = {
  args: {
    title:
      "これは非常に長いタイトルで、ヘッダーのレイアウトがどのように処理されるかをテストするためのものです",
    isPublished: true,
    isLoading: false,
  },
};
