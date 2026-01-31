import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { EditorHeader } from "@shared/components/organisms/common/editor";

const meta = {
  component: EditorHeader,
  args: {
    onTitleChange: () => {},
    onPublishChange: () => {},
    onSave: () => {},
  },
} satisfies Meta<typeof EditorHeader>;

export default meta;

export const ArticleNew: StoryObj<typeof EditorHeader> = {
  args: {
    title: "",
    isPublished: false,
    isSaving: false,
    type: "article",
  },
};

export const ArticleWithTitle: StoryObj<typeof EditorHeader> = {
  args: {
    title: "Next.js 15の新機能について",
    isPublished: false,
    isSaving: false,
    type: "article",
  },
};

export const ArticlePublished: StoryObj<typeof EditorHeader> = {
  args: {
    title: "Next.js 15の新機能について",
    isPublished: true,
    isSaving: false,
    type: "article",
  },
};

export const ArticleSaving: StoryObj<typeof EditorHeader> = {
  args: {
    title: "Next.js 15の新機能について",
    isPublished: false,
    isSaving: true,
    type: "article",
  },
};

export const ChapterNew: StoryObj<typeof EditorHeader> = {
  args: {
    title: "",
    isPublished: false,
    isSaving: false,
    type: "chapter",
  },
};

export const BookNew: StoryObj<typeof EditorHeader> = {
  args: {
    title: "",
    isPublished: false,
    isSaving: false,
    type: "book",
  },
};
