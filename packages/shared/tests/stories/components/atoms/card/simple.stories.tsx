import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SimpleCard } from "@shared/components/atoms/card/simple";

const meta = {
  component: SimpleCard,
} satisfies Meta<typeof SimpleCard>;

export default meta;

export const Default: StoryObj<typeof SimpleCard> = {
  args: {
    children: "シンプルなカードのコンテンツです",
    style: { padding: "1rem" },
  },
};

export const WithMultipleChildren: StoryObj<typeof SimpleCard> = {
  args: {
    style: { padding: "1rem" },
  },
  render: (args) => (
    <SimpleCard {...args}>
      <h3>カードタイトル</h3>
      <p>カードの説明文がここに入ります。</p>
    </SimpleCard>
  ),
};
