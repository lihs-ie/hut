import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import {
  AdminContentList,
  Props,
} from "@/app/admin/_components/molecules/list/content";
import { PublishStatus } from "@shared/domains/common";

const meta = {
  component: AdminContentList,
} satisfies Meta<typeof AdminContentList>;

export default meta;

const createContent = (index: number): Props["contents"][number] => ({
  title: `Sample Title ${index + 1}`,
  status:
    Object.values(PublishStatus)[index % Object.values(PublishStatus).length],
  previewHref: "#",
  editHref: "#",
  updatedAt: new Date(),
  onTerminate: async () => {
    console.log(`Terminate content ${index + 1}`);
  },
});

const createContents = (count: number) =>
  Array.from({ length: count }, (_, i) => createContent(i));

export const Default: StoryObj<typeof AdminContentList> = {
  args: {
    contents: createContents(5),
  },
};
