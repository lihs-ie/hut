import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SearchResultCard } from "@shared/components/molecules/list/card/search-result";
import { ContentType } from "@shared/domains/search-index/common";

const meta = {
  component: SearchResultCard,
} satisfies Meta<typeof SearchResultCard>;

export default meta;

export const Article: StoryObj<typeof SearchResultCard> = {
  args: {
    slug: "modern-web-application-development-best-practices",
    type: ContentType.ARTICLE,
    title: "モダンWebアプリケーション開発のベストプラクティス",
    tags: ["Next.js", "React", "TypeScript"],
    date: new Date("2024-01-15"),
  },
};

export const Scrap: StoryObj<typeof SearchResultCard> = {
  args: {
    slug: "handmade-japanese-phonemes",
    type: ContentType.MEMO,
    title: "日本語音素を手作りする",
    tags: ["Python", "AI", "機械学習"],
    date: new Date("2024-01-14"),
    commentCount: 12,
  },
};

export const Book: StoryObj<typeof SearchResultCard> = {
  args: {
    slug: "nextjs-react-design-guide",
    type: ContentType.SERIES,
    title: "Next.js 15 / React 19 実践設計ガイド",
    tags: ["Next.js", "React", "TypeScript"],
    date: new Date("2024-01-13"),
    chapterCount: 8,
  },
};

export const WithImage: StoryObj<typeof SearchResultCard> = {
  args: {
    slug: "design-system-ui-library",
    type: ContentType.ARTICLE,
    title: "デザインシステムの構築：UIコンポーネントライブラリ",
    tags: ["デザイン", "CSS", "UI/UX"],
    date: new Date("2024-01-12"),
    imageUrl: "https://picsum.photos/400/300",
  },
};

export const WithExcerpt: StoryObj<typeof SearchResultCard> = {
  args: {
    slug: "typescript-safe-application-design",
    type: ContentType.ARTICLE,
    title: "TypeScriptで型安全なアプリケーション設計",
    excerpt:
      "TypeScriptの型システムを活用して、より安全で保守性の高いアプリケーションを設計する方法を解説します。",
    tags: ["TypeScript", "Node.js"],
    date: new Date("2024-01-10"),
  },
};

export const FullExample: StoryObj<typeof SearchResultCard> = {
  args: {
    slug: "react-server-components-complete-guide",
    type: ContentType.ARTICLE,
    title: "React Server Componentsの完全ガイド",
    excerpt:
      "React Server Componentsの概念から実践的な使い方まで、詳しく解説します。パフォーマンス最適化のヒントも含まれています。",
    tags: ["React", "Next.js", "TypeScript"],
    date: new Date("2024-01-20"),
    imageUrl: "https://picsum.photos/400/300",
  },
};

export const LongTitle: StoryObj<typeof SearchResultCard> = {
  args: {
    slug: "long-title-article",
    type: ContentType.ARTICLE,
    title:
      "これは非常に長いタイトルの記事です。タイトルが長すぎる場合にどのように表示されるかをテストしています。複数行にわたる場合の挙動を確認します。",
    tags: ["テスト"],
    date: new Date("2024-01-01"),
  },
};
