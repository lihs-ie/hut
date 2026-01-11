import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { MonthInput } from "@shared/components/atoms/input/month";

const meta = {
  component: MonthInput,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof MonthInput>;

export default meta;

type Story = StoryObj<typeof MonthInput>;

export const Default: Story = {
  args: {},
};

export const WithValue: Story = {
  args: {
    value: "2024-01",
  },
};

export const Disabled: Story = {
  args: {
    value: "2024-01",
    disabled: true,
  },
};

export const Interactive: Story = {
  render: () => {
    const [value, setValue] = useState("");
    return (
      <div style={{ width: "200px" }}>
        <MonthInput
          value={value}
          onChange={(event) => setValue(event.target.value)}
        />
        <p style={{ marginTop: "1rem", color: "var(--muted-foreground)", fontSize: "0.875rem" }}>
          選択値: {value || "(未選択)"}
        </p>
      </div>
    );
  },
};
