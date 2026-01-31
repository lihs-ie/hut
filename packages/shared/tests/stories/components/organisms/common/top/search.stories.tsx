import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ContentSectionPresenter } from "@shared/components/organisms/common/top/search.presenter";
import { ContentType } from "@shared/domains/search-token";
import { slugSchema } from "@shared/domains/common";
import { tagNameSchema } from "@shared/domains/attributes/tag";

const meta = {
  component: ContentSectionPresenter,
} satisfies Meta<typeof ContentSectionPresenter>;

export default meta;

const createSampleContent = (
  type: (typeof ContentType)[keyof typeof ContentType],
  count: number
) =>
  Array.from({ length: count }, (_, index) => ({
    slug: slugSchema.parse(`sample-${type}-${index + 1}`),
    type,
    title: `${type === ContentType.ARTICLE ? "記事" : type === ContentType.MEMO ? "メモ" : "シリーズ"}タイトル ${index + 1}`,
    date: new Date(2024, 0, index + 1),
    tagNames: [
      tagNameSchema.parse("TypeScript"),
      tagNameSchema.parse("React"),
    ],
    excerpt: `これは${type}の抜粋テキストです。`,
  }));

export const Articles: StoryObj<typeof ContentSectionPresenter> = {
  args: {
    title: "最新の記事",
    type: ContentType.ARTICLE,
    content: createSampleContent(ContentType.ARTICLE, 6),
    viewAllLink: "/articles",
    maxItems: 6,
  },
};

export const Memos: StoryObj<typeof ContentSectionPresenter> = {
  args: {
    title: "最新のメモ",
    type: ContentType.MEMO,
    content: createSampleContent(ContentType.MEMO, 6),
    viewAllLink: "/memos",
    maxItems: 6,
  },
};

export const Series: StoryObj<typeof ContentSectionPresenter> = {
  args: {
    title: "シリーズ",
    type: ContentType.SERIES,
    content: createSampleContent(ContentType.SERIES, 4),
    viewAllLink: "/series",
    maxItems: 4,
  },
};

export const WithoutViewAllLink: StoryObj<typeof ContentSectionPresenter> = {
  args: {
    title: "記事一覧",
    type: ContentType.ARTICLE,
    content: createSampleContent(ContentType.ARTICLE, 3),
    maxItems: 3,
  },
};

export const LimitedItems: StoryObj<typeof ContentSectionPresenter> = {
  args: {
    title: "最新3件の記事",
    type: ContentType.ARTICLE,
    content: createSampleContent(ContentType.ARTICLE, 10),
    viewAllLink: "/articles",
    maxItems: 3,
  },
};

export const Empty: StoryObj<typeof ContentSectionPresenter> = {
  args: {
    title: "記事がありません",
    type: ContentType.ARTICLE,
    content: [],
    viewAllLink: "/articles",
    maxItems: 6,
  },
};
