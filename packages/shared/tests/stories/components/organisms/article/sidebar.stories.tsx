import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SidebarPresenter } from "@shared/components/organisms/article/sidebar.presenter";
import { generateToc } from "@shared/components/global/mdx";

const meta = {
  component: SidebarPresenter,
} satisfies Meta<typeof SidebarPresenter>;
export default meta;

const content = `
# Heading 1
Some content under heading 1.

## Heading 1.1
Some content under heading 1.1.

### Heading 1.1.1
Some content under heading 1.1.1.

## Heading 1.2
Some content under heading 1.2.

# Heading 2
Some content under heading 2.

## Heading 2.1
Some content under heading 2.1.
`;

export const Default: StoryObj<typeof SidebarPresenter> = {
  args: {
    root: generateToc(content),
  },
};
