import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminContentCard } from "../../../../../../../admin/src/app/admin/_components/molecules/list/card/content";
import { PublishStatus } from "@shared/domains/common";

const meta = {
  component: AdminContentCard,
  parameters: {
    layout: "padded",
  },
  argTypes: {
    status: {
      control: "select",
      options: [PublishStatus.DRAFT, PublishStatus.PUBLISHED, PublishStatus.ARCHIVED],
    },
  },
} satisfies Meta<typeof AdminContentCard>;

export default meta;

type Story = StoryObj<typeof AdminContentCard>;

const mockOnTerminate = () => {
  console.log("Terminate requested");
};

export const Draft: Story = {
  args: {
    title: "下書きの記事タイトル",
    status: PublishStatus.DRAFT,
    previewHref: "/articles/preview/1",
    editHref: "/admin/articles/1/edit",
    updatedAt: new Date("2024-01-15T10:30:00"),
    onTerminate: mockOnTerminate,
  },
};

export const Published: Story = {
  args: {
    title: "公開中の記事タイトル",
    status: PublishStatus.PUBLISHED,
    previewHref: "/articles/preview/2",
    editHref: "/admin/articles/2/edit",
    updatedAt: new Date("2024-01-14T15:45:00"),
    onTerminate: mockOnTerminate,
  },
};

export const Archived: Story = {
  args: {
    title: "アーカイブされた記事タイトル",
    status: PublishStatus.ARCHIVED,
    previewHref: "/articles/preview/3",
    editHref: "/admin/articles/3/edit",
    updatedAt: new Date("2024-01-10T08:00:00"),
    onTerminate: mockOnTerminate,
  },
};

export const LongTitle: Story = {
  args: {
    title:
      "これは非常に長いタイトルの記事です。タイトルが長すぎる場合にどのように表示されるかをテストしています。UIが適切に処理されることを確認します。",
    status: PublishStatus.PUBLISHED,
    previewHref: "/articles/preview/4",
    editHref: "/admin/articles/4/edit",
    updatedAt: new Date("2024-01-13T12:00:00"),
    onTerminate: mockOnTerminate,
  },
};

export const RecentlyUpdated: Story = {
  args: {
    title: "最近更新された記事",
    status: PublishStatus.DRAFT,
    previewHref: "/articles/preview/5",
    editHref: "/admin/articles/5/edit",
    updatedAt: new Date(),
    onTerminate: mockOnTerminate,
  },
};
