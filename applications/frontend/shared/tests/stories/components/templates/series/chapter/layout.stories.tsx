import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ChapterLayoutTemplate } from "@shared/components/templates/series/chapter/layout";

const meta = {
  component: ChapterLayoutTemplate,
} satisfies Meta<typeof ChapterLayoutTemplate>;

export default meta;

const TocPlaceholder = () => (
  <div
    style={{
      width: 280,
      padding: "1.5rem 1rem",
      borderRight: "1px solid var(--border)",
      background: "var(--background-secondary)",
    }}
  >
    <p style={{ fontSize: "0.875rem", fontWeight: 600 }}>目次（サイドバー）</p>
  </div>
);

const ContentPlaceholder = () => (
  <div style={{ padding: "2rem", maxWidth: "52rem" }}>
    <h1>チャプタータイトル</h1>
    <p>本文コンテンツがここに表示されます。</p>
  </div>
);

export const Default: StoryObj<typeof ChapterLayoutTemplate> = {
  args: {
    toc: <TocPlaceholder />,
    children: <ContentPlaceholder />,
  },
};
