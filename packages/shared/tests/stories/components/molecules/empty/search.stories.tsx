import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SearchEmpty } from "@shared/components/molecules/empty/search";

const meta = {
  component: SearchEmpty,
} satisfies Meta<typeof SearchEmpty>;

export default meta;

export const Initial: StoryObj<typeof SearchEmpty> = {
  args: {
    variant: "initial",
  },
};

export const Empty: StoryObj<typeof SearchEmpty> = {
  args: {
    variant: "empty",
  },
};

export const Error: StoryObj<typeof SearchEmpty> = {
  args: {
    variant: "error",
    onRetry: () => {},
  },
};

export const DefaultVariant: StoryObj<typeof SearchEmpty> = {
  args: {},
};
