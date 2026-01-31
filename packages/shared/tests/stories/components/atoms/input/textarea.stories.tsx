import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { Textarea } from "@shared/components/atoms/input/textarea";

const meta = {
  component: Textarea,
} satisfies Meta<typeof Textarea>;

export default meta;

export const Default: StoryObj<typeof Textarea> = {
  args: {},
};

export const WithPlaceholder: StoryObj<typeof Textarea> = {
  args: {
    placeholder: "テキストを入力してください",
  },
};

export const WithValue: StoryObj<typeof Textarea> = {
  args: {
    value: "入力済みのテキスト\n複数行にわたる内容を\n入力することができます。",
    placeholder: "テキストを入力してください",
  },
};

export const CustomRows: StoryObj<typeof Textarea> = {
  args: {
    placeholder: "8行のテキストエリア",
    rows: 8,
  },
};

export const Disabled: StoryObj<typeof Textarea> = {
  args: {
    value: "無効状態のテキストエリア",
    disabled: true,
  },
};

export const Interactive: StoryObj<typeof Textarea> = {
  render: () => {
    const [value, setValue] = useState("");
    return (
      <div style={{ width: "400px" }}>
        <Textarea
          placeholder="入力してください"
          value={value}
          onChange={(event) => setValue(event.target.value)}
          rows={6}
        />
        <p style={{ marginTop: "1rem", color: "var(--muted-foreground)", fontSize: "0.875rem" }}>
          文字数: {value.length}
        </p>
      </div>
    );
  },
};
