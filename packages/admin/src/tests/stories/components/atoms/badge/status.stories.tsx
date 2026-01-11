import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PublishStatusBadge } from "@/app/admin/_components/atoms/badge/status";
import { PublishStatus } from "@shared/domains/common";

const meta = {
  component: PublishStatusBadge,
} satisfies Meta<typeof PublishStatusBadge>;
export default meta;

export const Default: StoryObj<typeof PublishStatusBadge> = {
  args: {
    status: PublishStatus.DRAFT,
  },
};

export const Published: StoryObj<typeof PublishStatusBadge> = {
  args: {
    status: PublishStatus.PUBLISHED,
  },
};

export const Archived: StoryObj<typeof PublishStatusBadge> = {
  args: {
    status: PublishStatus.ARCHIVED,
  },
};
