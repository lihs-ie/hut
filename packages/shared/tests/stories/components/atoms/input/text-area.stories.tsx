import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TextArea } from "@shared/components/atoms/input/text-area";

const meta = {
  component: TextArea,
} satisfies Meta<typeof TextArea>;

export default meta;

export const Default: StoryObj<typeof TextArea> = {
  args: {
    value: "",
    placeholder: "テキストを入力",
    onChange: () => {},
  },
};

export const WithValue: StoryObj<typeof TextArea> = {
  args: {
    value: "サンプルテキスト",
    onChange: () => {},
  },
};

export const Disabled: StoryObj<typeof TextArea> = {
  args: {
    value: "無効化されたテキスト",
    disabled: true,
    onChange: () => {},
  },
};
