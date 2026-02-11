import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminContentListIndex } from "@/app/admin/_components/templates/admin/content/list";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../../../shared/tests/support/molds/domains/attributes";
import { PublishStatus } from "@shared/domains/common";
import { ContentType } from "@shared/domains/search-token";

const meta = {
  component: AdminContentListIndex,
  parameters: {
    nextjs: {
      appDirectory: true,
    },
  },
} satisfies Meta<typeof AdminContentListIndex>;

export default meta;

const tags = Forger(TagMold).forgeMulti(20);

type Content = {
  title: string;
  status: PublishStatus;
  previewHref: string;
  editHref: string;
  updatedAt: Date;
  onTerminate: () => Promise<void>;
};

const contents = (count: number) =>
  Array.from({ length: count }, (_, i) => ({
    title: `コンテンツタイトル ${i + 1}`,
    status:
      Object.values(PublishStatus)[i % Object.values(PublishStatus).length],
    previewHref: `#preview-${i + 1}`,
    editHref: `#edit-${i + 1}`,
    updatedAt: new Date(Date.now() - i * 86400000),
    onTerminate: async () => {
      console.log(`Terminate content ${i + 1}`);
    },
  }));

export const Default: StoryObj<typeof AdminContentListIndex<Content, object>> =
  {
    args: {
      title: "サンプルコンテンツ一覧",
      getAllTags: async () => tags,
      searchContents: async () => contents(15),
      unvalidated: {},
      contentType: ContentType.ARTICLE,
      valuesOf: (content: Content) => ({
        title: content.title,
        status: content.status,
        previewHref: content.previewHref,
        editHref: content.editHref,
        updatedAt: content.updatedAt,
        onTerminate: content.onTerminate,
      }),
    },
  };
