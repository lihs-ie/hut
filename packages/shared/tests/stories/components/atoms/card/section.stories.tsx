import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SectionCard } from "@shared/components/atoms/card/section";

const meta = {
  component: SectionCard,
} satisfies Meta<typeof SectionCard>;

export default meta;

export const Default: StoryObj<typeof SectionCard> = {
  args: {
    title: "Section Title",
    children: <p>This is the content of the section card.</p>,
  },
};
