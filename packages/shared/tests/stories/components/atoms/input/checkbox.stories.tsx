import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { Checkbox } from "@shared/components/atoms/input/checkbox";

const meta = {
  component: Checkbox,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof Checkbox>;

export default meta;

type Story = StoryObj<typeof Checkbox>;

export const Default: Story = {
  args: {
    label: "チェックボックス",
  },
};

export const Checked: Story = {
  args: {
    label: "チェック済み",
    checked: true,
  },
};

export const WithoutLabel: Story = {
  args: {
    checked: false,
  },
};

export const Disabled: Story = {
  args: {
    label: "無効状態",
    disabled: true,
  },
};

export const DisabledChecked: Story = {
  args: {
    label: "無効状態（チェック済み）",
    checked: true,
    disabled: true,
  },
};

export const Interactive: Story = {
  render: () => {
    const [checked, setChecked] = useState(false);
    return (
      <div>
        <Checkbox
          label="現在も使用中"
          checked={checked}
          onChange={(event) => setChecked(event.target.checked)}
        />
        <p style={{ marginTop: "1rem", color: "var(--muted-foreground)", fontSize: "0.875rem" }}>
          状態: {checked ? "チェック済み" : "未チェック"}
        </p>
      </div>
    );
  },
};
