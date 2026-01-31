import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { FormButton } from "@shared/components/atoms/button/form";

const meta = {
  component: FormButton,
} satisfies Meta<typeof FormButton>;

export default meta;

export const Default: StoryObj<typeof FormButton> = {
  args: {
    children: "送信",
  },
};

export const Disabled: StoryObj<typeof FormButton> = {
  args: {
    children: "送信",
    disabled: true,
  },
};

export const WithLongText: StoryObj<typeof FormButton> = {
  args: {
    children: "フォームを送信する",
  },
};
