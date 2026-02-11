import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { FileTextIcon } from "@shared/components/atoms/icon/file-text";

const meta = {
  component: FileTextIcon,
} satisfies Meta<typeof FileTextIcon>;

export default meta;

export const Default: StoryObj<typeof FileTextIcon> = {};
