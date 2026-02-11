import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { SectionHeader } from "@shared/components/atoms/text/section";

const meta = {
  component: SectionHeader,
} satisfies Meta<typeof SectionHeader>;

export default meta;

export const Default: StoryObj<typeof SectionHeader> = {
  args: {
    title: "PV推移",
  },
};

export const WithDescription: StoryObj<typeof SectionHeader> = {
  args: {
    title: "PV推移",
    description: "日別のページビュー数の推移",
  },
};
