import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TechStackList } from "@shared/components/molecules/list/tech-stack";

const meta = {
  component: TechStackList,
} satisfies Meta<typeof TechStackList>;

export default meta;

export const Default: StoryObj<typeof TechStackList> = {
  args: {
    techStacks: new Map([
      [
        "frontend",
        [
          { from: new Date("2020-01-01"), name: "React", logo: "/images/react.png" },
          { from: new Date("2021-01-01"), name: "Vue.js", logo: "/images/vue.png" },
          { from: new Date("2022-01-01"), name: "Next.js", logo: "/images/next.png" },
        ],
      ],
      [
        "backend",
        [
          { from: new Date("2019-01-01"), name: "Node.js", logo: "/images/node.png" },
          { from: new Date("2020-06-01"), name: "Go", logo: "/images/go.png" },
          { from: new Date("2021-06-01"), name: "Python", logo: "/images/python.png" },
        ],
      ],
    ]),
    now: new Date("2024-01-01"),
  },
};
