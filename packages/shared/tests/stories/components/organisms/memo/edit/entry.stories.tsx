import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { EntryEditor } from "@shared/components/organisms/memo/edit/entry";

const meta = {
  component: EntryEditor,
} satisfies Meta<typeof EntryEditor>;

export default meta;

export const Default: StoryObj<typeof EntryEditor> = {};
