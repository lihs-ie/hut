import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticleEdit } from "@shared/components/templates/article/edit";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ArticleMold,
  TitleMold,
  SlugMold,
  ContentMold,
} from "../../../../support/molds/domains/article";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";
import { PublishStatus } from "@shared/domains/common";

const meta = {
  component: ArticleEdit,
  parameters: {
    nextjs: {
      appDirectory: true,
    },
  },
} satisfies Meta<typeof ArticleEdit>;

export default meta;

const tags = Forger(TagMold).forgeMultiWithSeed(10, 1);

const persist = async () => {
  console.log("Article persisted");
};

const uploadImage = async (_file: File | Blob, path: string) => {
  console.log("Image uploaded to", path);
  return `https://example.com/images/${path}`;
};

export const NewArticle: StoryObj<typeof ArticleEdit> = {
  args: {
    tags,
    persist,
    uploadImage,
  },
};

const articleContent = `---
title: "Next.js 15の新機能について"
slug: "nextjs-15-features"
excerpt: "Next.js 15で追加された新機能を紹介します"
tags: []
---

## はじめに

Next.js 15がリリースされました。この記事では、主要な新機能について解説します。

## Server Actions の改善

Server Actionsがより使いやすくなりました。

\`\`\`typescript
async function submitForm(formData: FormData) {
  "use server";
  // サーバー側で処理
}
\`\`\`

## まとめ

Next.js 15は多くの改善が含まれています。
`;

const existingArticle = Forger(ArticleMold).forge({
  title: Forger(TitleMold).forge({ value: "Next.js 15の新機能について" }),
  slug: Forger(SlugMold).forge({ value: "nextjs-15-features" }),
  content: Forger(ContentMold).forge({ value: articleContent }),
  status: PublishStatus.DRAFT,
  tags: [tags[0].identifier, tags[1].identifier],
});

export const EditExistingArticle: StoryObj<typeof ArticleEdit> = {
  args: {
    initial: existingArticle,
    tags,
    persist,
    uploadImage,
  },
};

const publishedContent = `---
title: "公開済みの記事"
slug: "published-article"
excerpt: "これは公開済みの記事です"
tags: []
---

## 公開済みコンテンツ

この記事は公開されています。
`;

const publishedArticle = Forger(ArticleMold).forge({
  title: Forger(TitleMold).forge({ value: "公開済みの記事" }),
  slug: Forger(SlugMold).forge({ value: "published-article" }),
  content: Forger(ContentMold).forge({ value: publishedContent }),
  status: PublishStatus.PUBLISHED,
  tags: [tags[2].identifier],
});

export const EditPublishedArticle: StoryObj<typeof ArticleEdit> = {
  args: {
    initial: publishedArticle,
    tags,
    persist,
    uploadImage,
  },
};
