import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { FormLabel } from "@shared/components/atoms/text/form-label";

const meta = {
  component: FormLabel,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof FormLabel>;

export default meta;

type Story = StoryObj<typeof FormLabel>;

export const Default: Story = {
  args: {
    children: "ラベル",
  },
};

export const Required: Story = {
  args: {
    children: "表示名",
    required: true,
  },
};

export const WithHtmlFor: Story = {
  args: {
    children: "メールアドレス",
    htmlFor: "email",
    required: true,
  },
};
