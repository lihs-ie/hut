import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { FormField } from "@shared/components/molecules/form/field";
import { TextInput } from "@shared/components/atoms/input/text";
import { Textarea } from "@shared/components/atoms/input/textarea";

const meta = {
  component: FormField,
  parameters: {
    layout: "centered",
  },
  decorators: [
    (Story) => (
      <div style={{ width: "320px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof FormField>;

export default meta;

type Story = StoryObj<typeof FormField>;

export const Default: Story = {
  args: {
    label: "表示名",
    children: <TextInput placeholder="名前を入力" />,
  },
};

export const Required: Story = {
  args: {
    label: "表示名",
    required: true,
    children: <TextInput placeholder="名前を入力" />,
  },
};

export const WithNote: Story = {
  args: {
    label: "GitHubユーザー名",
    note: "ユーザー名だけを入力してください",
    children: <TextInput placeholder="username" />,
  },
};

export const WithTextarea: Story = {
  args: {
    label: "自己紹介",
    note: "500文字以内で入力してください",
    children: <Textarea placeholder="自己紹介を入力" rows={4} />,
  },
};
