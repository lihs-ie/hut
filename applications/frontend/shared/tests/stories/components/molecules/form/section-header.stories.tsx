import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SectionHeader } from "@shared/components/molecules/form/section-header";
import { PlusIcon } from "@shared/components/atoms/icon/plus";

const meta = {
  component: SectionHeader,
  parameters: {
    layout: "padded",
  },
  decorators: [
    (Story) => (
      <div style={{ maxWidth: "600px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof SectionHeader>;

export default meta;

type Story = StoryObj<typeof SectionHeader>;

export const Default: Story = {
  args: {
    title: "技術スタック",
  },
};

export const WithButton: Story = {
  args: {
    title: "技術スタック",
    children: (
      <button
        type="button"
        style={{
          display: "inline-flex",
          alignItems: "center",
          gap: "0.375rem",
          padding: "0.375rem 0.75rem",
          fontSize: "0.875rem",
          fontWeight: 500,
          color: "var(--foreground)",
          background: "var(--background)",
          border: "1px solid var(--border)",
          borderRadius: "var(--radius-md)",
          cursor: "pointer",
        }}
      >
        <PlusIcon className="icon-sm" />
        追加
      </button>
    ),
  },
};

export const CareerSection: Story = {
  args: {
    title: "経歴",
    children: (
      <button
        type="button"
        style={{
          display: "inline-flex",
          alignItems: "center",
          gap: "0.375rem",
          padding: "0.375rem 0.75rem",
          fontSize: "0.875rem",
          fontWeight: 500,
          color: "var(--foreground)",
          background: "var(--background)",
          border: "1px solid var(--border)",
          borderRadius: "var(--radius-md)",
          cursor: "pointer",
        }}
      >
        <PlusIcon className="icon-sm" />
        追加
      </button>
    ),
  },
};
