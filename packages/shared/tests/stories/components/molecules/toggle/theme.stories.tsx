import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Theme, ThemeToggle } from "@shared/components/molecules/toggle/theme";
import { useState } from "react";

const meta = {
  component: ThemeToggle,
} satisfies Meta<typeof ThemeToggle>;

export default meta;

export const Default: StoryObj<typeof ThemeToggle> = {
  args: {
    value: "light",
  },
  render: (args) => {
    const [value, setValue] = useState<Theme>(args.value);

    return (
      <ThemeToggle
        {...args}
        value={value}
        onToggle={() => setValue(value === "light" ? "dark" : "light")}
      />
    );
  },
};
