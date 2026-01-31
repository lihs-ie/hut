import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { NumberInput } from "@shared/components/atoms/input/number";

const meta = {
  component: NumberInput,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof NumberInput>;

export default meta;

type Story = StoryObj<typeof NumberInput>;

export const Default: Story = {
  args: {},
};

export const WithValue: Story = {
  args: {
    value: 5,
  },
};

export const WithPlaceholder: Story = {
  args: {
    placeholder: "0.0",
  },
};

export const WithStep: Story = {
  args: {
    value: 2.5,
    step: 0.5,
    min: 0,
  },
};

export const WithMinMax: Story = {
  args: {
    value: 50,
    min: 0,
    max: 100,
  },
};

export const Disabled: Story = {
  args: {
    value: 10,
    disabled: true,
  },
};

export const Interactive: Story = {
  render: () => {
    const [value, setValue] = useState(0);
    return (
      <div style={{ width: "150px" }}>
        <NumberInput
          value={value}
          onChange={(event) => setValue(Number.parseFloat(event.target.value) || 0)}
          step={0.5}
          min={0}
          placeholder="経験年数"
        />
        <p style={{ marginTop: "1rem", color: "var(--muted-foreground)", fontSize: "0.875rem" }}>
          値: {value}年
        </p>
      </div>
    );
  },
};
