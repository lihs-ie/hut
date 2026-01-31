import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PublishStatusBadge } from "../../../../../../../admin/src/app/admin/_components/atoms/badge/status";
import { PublishStatus } from "@shared/domains/common";

const meta = {
  component: PublishStatusBadge,
  parameters: {
    layout: "centered",
  },
  argTypes: {
    status: {
      control: "select",
      options: [PublishStatus.DRAFT, PublishStatus.PUBLISHED, PublishStatus.ARCHIVED],
    },
  },
} satisfies Meta<typeof PublishStatusBadge>;

export default meta;

type Story = StoryObj<typeof PublishStatusBadge>;

export const Draft: Story = {
  args: {
    status: PublishStatus.DRAFT,
  },
};

export const Published: Story = {
  args: {
    status: PublishStatus.PUBLISHED,
  },
};

export const Archived: Story = {
  args: {
    status: PublishStatus.ARCHIVED,
  },
};

export const AllStatuses: Story = {
  render: () => (
    <div style={{ display: "flex", gap: "0.5rem", alignItems: "center" }}>
      <PublishStatusBadge status={PublishStatus.DRAFT} />
      <PublishStatusBadge status={PublishStatus.PUBLISHED} />
      <PublishStatusBadge status={PublishStatus.ARCHIVED} />
    </div>
  ),
};
