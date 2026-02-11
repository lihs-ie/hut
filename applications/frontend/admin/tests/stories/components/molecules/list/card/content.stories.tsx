import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminContentCard } from "@/app/admin/_components/molecules/list/card/content";
import { PublishStatus } from "@shared/domains/common";

const meta = {
  component: AdminContentCard,
} satisfies Meta<typeof AdminContentCard>;

export default meta;

export const Default: StoryObj<typeof AdminContentCard> = {
  args: {
    title: "Sample Title",
    status: PublishStatus.DRAFT,
    previewHref: "#",
    editHref: "#",
    updatedAt: new Date(),
  },
};

export const Published: StoryObj<typeof AdminContentCard> = {
  args: {
    title: "Published Content",
    status: PublishStatus.PUBLISHED,
    previewHref: "#",
    editHref: "#",
    updatedAt: new Date(),
  },
};

export const Archived: StoryObj<typeof AdminContentCard> = {
  args: {
    title: "Archived Content",
    status: PublishStatus.ARCHIVED,
    previewHref: "#",
    editHref: "#",
    updatedAt: new Date(),
  },
};

export const LongTitle: StoryObj<typeof AdminContentCard> = {
  args: {
    title:
      "This is a very long title to test how the content card handles overflow in the title section",
    status: PublishStatus.DRAFT,
    previewHref: "#",
    editHref: "#",
    updatedAt: new Date(),
  },
};
