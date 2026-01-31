import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { SearchInput } from "@shared/components/molecules/form/search-input";

const meta = {
  component: SearchInput,
} satisfies Meta<typeof SearchInput>;

export default meta;

export const Default: StoryObj<typeof SearchInput> = {
  args: {
    placeholder: "検索",
    value: "",
    onChange: () => {},
  },
};

export const WithPlaceholder: StoryObj<typeof SearchInput> = {
  args: {
    placeholder: "タイトルやトピックで検索",
    value: "",
    onChange: () => {},
  },
};

export const WithValue: StoryObj<typeof SearchInput> = {
  args: {
    placeholder: "検索",
    value: "Next.js",
    onChange: () => {},
  },
};

export const Interactive: StoryObj<typeof SearchInput> = {
  render: () => {
    const [value, setValue] = useState("");
    return (
      <div style={{ width: "400px" }}>
        <SearchInput
          placeholder="タイトルやトピックで検索"
          value={value}
          onChange={setValue}
        />
        <p style={{ marginTop: "1rem", color: "var(--muted-foreground)" }}>
          入力値: {value || "(空)"}
        </p>
      </div>
    );
  },
};
