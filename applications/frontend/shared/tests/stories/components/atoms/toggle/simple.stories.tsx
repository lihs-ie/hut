import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { SimpleSwitch } from "@shared/components/atoms/toggle/simple";

const meta = {
  component: SimpleSwitch,
} satisfies Meta<typeof SimpleSwitch>;

export default meta;

export const Off: StoryObj<typeof SimpleSwitch> = {
  args: {
    checked: false,
    onChange: () => {},
  },
};

export const On: StoryObj<typeof SimpleSwitch> = {
  args: {
    checked: true,
    onChange: () => {},
  },
};

export const Interactive: StoryObj<typeof SimpleSwitch> = {
  render: () => {
    const InteractiveSwitch = () => {
      const [checked, setChecked] = useState(false);
      return <SimpleSwitch checked={checked} onChange={setChecked} />;
    };
    return <InteractiveSwitch />;
  },
};
