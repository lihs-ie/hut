import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticleTitleSkeleton, ArticleContentSkeleton, ArticleSidebarSkeleton } from "@shared/components/molecules/skeleton/article";

const meta = {
  component: ArticleTitleSkeleton,
} satisfies Meta<typeof ArticleTitleSkeleton>;

export default meta;

export const Title: StoryObj<typeof ArticleTitleSkeleton> = {};

export const Content: StoryObj = {
  render: () => <ArticleContentSkeleton />,
};

export const Sidebar: StoryObj = {
  render: () => <ArticleSidebarSkeleton />,
};

export const FullLayout: StoryObj = {
  render: () => (
    <div style={{ display: "flex", gap: "2rem" }}>
      <div style={{ flex: 1 }}>
        <ArticleTitleSkeleton />
        <ArticleContentSkeleton />
      </div>
      <div style={{ width: "280px" }}>
        <ArticleSidebarSkeleton />
      </div>
    </div>
  ),
};
