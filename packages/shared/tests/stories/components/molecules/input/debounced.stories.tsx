import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { DebouncedTextInput } from "@shared/components/molecules/input/debounced";

const meta = {
  component: DebouncedTextInput,
} satisfies Meta<typeof DebouncedTextInput>;

export default meta;

export const Default: StoryObj<typeof DebouncedTextInput> = {
  args: {
    placeholder: "テキストを入力...",
    onDebouncedChange: (value: string) => console.log("Debounced value:", value),
  },
};

export const WithDefaultValue: StoryObj<typeof DebouncedTextInput> = {
  args: {
    defaultValue: "初期値",
    placeholder: "テキストを入力...",
    onDebouncedChange: (value: string) => console.log("Debounced value:", value),
  },
};

export const SearchType: StoryObj<typeof DebouncedTextInput> = {
  args: {
    type: "search",
    placeholder: "検索...",
    onDebouncedChange: (value: string) => console.log("Search value:", value),
  },
};

export const Disabled: StoryObj<typeof DebouncedTextInput> = {
  args: {
    placeholder: "無効化されています",
    disabled: true,
    onDebouncedChange: () => {},
  },
};

export const Interactive: StoryObj<typeof DebouncedTextInput> = {
  render: () => {
    const InteractiveInput = () => {
      const [debouncedValue, setDebouncedValue] = useState("");
      return (
        <div>
          <DebouncedTextInput
            placeholder="タイプすると値が更新されます（300ms遅延）"
            onDebouncedChange={setDebouncedValue}
            delay={300}
          />
          <p style={{ marginTop: "1rem" }}>デバウンスされた値: {debouncedValue}</p>
        </div>
      );
    };
    return <InteractiveInput />;
  },
};
