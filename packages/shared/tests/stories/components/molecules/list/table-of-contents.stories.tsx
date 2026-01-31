import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TableOfContents } from "@shared/components/molecules/list/table-of-contents";
import { generateToc } from "@shared/components/global/mdx";

const meta = {
  component: TableOfContents,
} satisfies Meta<typeof TableOfContents>;

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

const tocTree = generateToc(content);

export const Default: StoryObj<typeof TableOfContents> = {
  args: {
    root: tocTree,
  },
};
