import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminContentListPresenter } from "../../../../../../../admin/src/app/admin/_components/molecules/list/content.presenter";
import { PublishStatus } from "@shared/domains/common";

const meta = {
  component: AdminContentListPresenter,
  parameters: {
    layout: "padded",
  },
} satisfies Meta<typeof AdminContentListPresenter>;

export default meta;

type Story = StoryObj<typeof AdminContentListPresenter>;

const createContent = (
  title: string,
  status: typeof PublishStatus.DRAFT | typeof PublishStatus.PUBLISHED | typeof PublishStatus.ARCHIVED,
  id: string,
  updatedAt: Date
) => ({
  title,
  status,
  previewHref: `/articles/preview/${id}`,
  editHref: `/admin/articles/${id}/edit`,
  updatedAt,
  onTerminate: async () => {
    console.log(`Terminate requested for: ${title}`);
  },
});

export const Default: Story = {
  args: {
    contents: [
      createContent("Next.js 15の新機能について", PublishStatus.PUBLISHED, "1", new Date("2024-01-15")),
      createContent("TypeScriptの型推論を深掘りする", PublishStatus.DRAFT, "2", new Date("2024-01-14")),
      createContent("React Server Componentsの実装パターン", PublishStatus.PUBLISHED, "3", new Date("2024-01-13")),
    ],
  },
};

export const Empty: Story = {
  args: {
    contents: [],
  },
};

export const SingleItem: Story = {
  args: {
    contents: [
      createContent("単一の記事", PublishStatus.PUBLISHED, "1", new Date("2024-01-15")),
    ],
  },
};

export const MixedStatuses: Story = {
  args: {
    contents: [
      createContent("公開記事1", PublishStatus.PUBLISHED, "1", new Date("2024-01-15")),
      createContent("下書き記事1", PublishStatus.DRAFT, "2", new Date("2024-01-14")),
      createContent("アーカイブ記事1", PublishStatus.ARCHIVED, "3", new Date("2024-01-13")),
      createContent("公開記事2", PublishStatus.PUBLISHED, "4", new Date("2024-01-12")),
      createContent("下書き記事2", PublishStatus.DRAFT, "5", new Date("2024-01-11")),
    ],
  },
};

export const ManyItems: Story = {
  args: {
    contents: Array.from({ length: 10 }, (_, index) =>
      createContent(
        `記事タイトル ${index + 1}`,
        index % 3 === 0 ? PublishStatus.PUBLISHED : index % 3 === 1 ? PublishStatus.DRAFT : PublishStatus.ARCHIVED,
        String(index + 1),
        new Date(Date.now() - index * 24 * 60 * 60 * 1000)
      )
    ),
  },
};
