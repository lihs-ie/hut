import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { TextInput } from "@shared/components/atoms/input/text";

const meta = {
  component: TextInput,
} satisfies Meta<typeof TextInput>;

export default meta;

export const Default: StoryObj<typeof TextInput> = {
  args: {},
};

export const WithPlaceholder: StoryObj<typeof TextInput> = {
  args: {
    placeholder: "テキストを入力してください",
  },
};

export const WithValue: StoryObj<typeof TextInput> = {
  args: {
    value: "入力済みのテキスト",
    placeholder: "テキストを入力してください",
  },
};

export const Search: StoryObj<typeof TextInput> = {
  args: {
    type: "search",
    placeholder: "検索...",
  },
};

export const Disabled: StoryObj<typeof TextInput> = {
  args: {
    value: "無効状態",
    disabled: true,
  },
};

export const Interactive: StoryObj<typeof TextInput> = {
  render: () => {
    const [value, setValue] = useState("");
    return (
      <div style={{ width: "300px" }}>
        <TextInput
          placeholder="入力してください"
          value={value}
          onChange={(event) => setValue(event.target.value)}
        />
        <p style={{ marginTop: "1rem", color: "var(--muted-foreground)", fontSize: "0.875rem" }}>
          入力値: {value || "(空)"}
        </p>
      </div>
    );
  },
};
