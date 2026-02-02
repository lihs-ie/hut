import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { EraserButton } from "../../../../../../../admin/src/app/admin/_components/molecules/button/eraser";

const meta = {
  component: EraserButton,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof EraserButton>;

export default meta;

type Story = StoryObj<typeof EraserButton>;

const mockOnClick = () => {
  console.log("Eraser button clicked");
};

export const Default: Story = {
  args: {
    onClick: mockOnClick,
  },
};

export const WithTypeButton: Story = {
  args: {
    onClick: mockOnClick,
    type: "button",
  },
};

export const WithTypeSubmit: Story = {
  args: {
    onClick: mockOnClick,
    type: "submit",
  },
};

export const Disabled: Story = {
  args: {
    onClick: mockOnClick,
    disabled: true,
  },
};
