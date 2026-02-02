import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { SimpleSelect } from "@shared/components/atoms/select/simple";

const meta = {
  component: SimpleSelect,
} satisfies Meta<typeof SimpleSelect>;

export default meta;

export const Default: StoryObj<typeof SimpleSelect> = {
  args: {
    value: "option1",
    onChange: () => {},
    children: (
      <>
        <option value="option1">オプション 1</option>
        <option value="option2">オプション 2</option>
        <option value="option3">オプション 3</option>
      </>
    ),
  },
};

export const Interactive: StoryObj<typeof SimpleSelect> = {
  render: () => {
    const InteractiveSelect = () => {
      const [value, setValue] = useState("option1");
      return (
        <SimpleSelect value={value} onChange={setValue}>
          <option value="option1">オプション 1</option>
          <option value="option2">オプション 2</option>
          <option value="option3">オプション 3</option>
        </SimpleSelect>
      );
    };
    return <InteractiveSelect />;
  },
};
