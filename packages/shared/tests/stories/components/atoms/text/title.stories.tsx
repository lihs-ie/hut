import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TitleCard } from "@shared/components/molecules/text/title";
import { Builder } from "../../../../support/molds";
import { TimelineFactory } from "../../../../support/molds/domains/common/date";

const meta = {
  component: TitleCard,
} satisfies Meta<typeof TitleCard>;

export default meta;

export const Default: StoryObj<typeof TitleCard> = {
  args: {
    title: "Card Title",
    timeline: Builder(TimelineFactory).build(),
  },
};
