import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { EntryEditor } from "@shared/components/organisms/memo/edit/entry";

const meta = {
  component: EntryEditor,
  parameters: {
    nextjs: { appDirectory: true },
  },
  args: {
    persist: async (unvalidated, slug) => {
      console.log("Persisting entry:", unvalidated, "for slug:", slug);
    },
    slug: "sample-memo-slug",
    uploadAction: async (file, path) => {
      console.log("Uploading file:", file, "to path:", path);
      return `https://example.com/uploads/${path}`;
    },
  },
} satisfies Meta<typeof EntryEditor>;

export default meta;

export const Default: StoryObj<typeof EntryEditor> = {};
