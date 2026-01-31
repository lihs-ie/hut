import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { DropdownSelect, type CustomSelectOption } from "@shared/components/atoms/select/dropdown";
import { EyeIcon } from "@shared/components/atoms/icon/eye";
import { LockIcon } from "@shared/components/atoms/icon/lock";
import { GlobeIcon } from "@shared/components/atoms/icon/globe";

const meta = {
  component: DropdownSelect,
} satisfies Meta<typeof DropdownSelect>;

export default meta;

const simpleOptions: CustomSelectOption[] = [
  { value: "option1", label: "オプション 1" },
  { value: "option2", label: "オプション 2" },
  { value: "option3", label: "オプション 3" },
];

const optionsWithIcons: CustomSelectOption[] = [
  { value: "public", label: "公開", icon: <GlobeIcon />, description: "誰でも閲覧可能" },
  { value: "unlisted", label: "限定公開", icon: <EyeIcon />, description: "URLを知っている人のみ" },
  { value: "private", label: "非公開", icon: <LockIcon />, description: "自分のみ閲覧可能" },
];

export const Default: StoryObj<typeof DropdownSelect> = {
  args: {
    value: "option1",
    onChange: () => {},
    options: simpleOptions,
    name: "simple-select",
  },
};

export const WithIcons: StoryObj<typeof DropdownSelect> = {
  args: {
    value: "public",
    onChange: () => {},
    options: optionsWithIcons,
    name: "visibility-select",
  },
};

export const Interactive: StoryObj<typeof DropdownSelect> = {
  render: () => {
    const InteractiveDropdown = () => {
      const [value, setValue] = useState("public");
      return (
        <DropdownSelect
          value={value}
          onChange={setValue}
          options={optionsWithIcons}
          name="interactive-select"
        />
      );
    };
    return <InteractiveDropdown />;
  },
};
