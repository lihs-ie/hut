import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TechStackCard } from "@shared/components/molecules/list/card/tech-stack";

const meta = {
  component: TechStackCard,
} satisfies Meta<typeof TechStackCard>;

export default meta;

export const Default: StoryObj<typeof TechStackCard> = {
  args: {
    from: new Date("2020-01-01"),
    now: new Date("2024-01-01"),
    techStackName: "React",
    techStackLogo: "/images/react-logo.png",
  },
};
