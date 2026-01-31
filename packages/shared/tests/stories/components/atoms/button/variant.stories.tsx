import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { FileTextIcon } from "@shared/components/atoms/icon";

import { VariantButton } from "@shared/components/atoms/button/variant";

const meta = {
  component: VariantButton,
} satisfies Meta<typeof VariantButton>;

export default meta;

export const Default: StoryObj<typeof VariantButton> = {
  args: {
    children: "ボタン",
  },
};

export const Outline: StoryObj<typeof VariantButton> = {
  args: {
    variant: "outline",
    children: "アウトライン",
  },
};

export const Ghost: StoryObj<typeof VariantButton> = {
  args: {
    variant: "ghost",
    children: "ゴースト",
  },
};

export const Small: StoryObj<typeof VariantButton> = {
  args: {
    size: "sm",
    children: "小サイズ",
  },
};

export const SmallOutline: StoryObj<typeof VariantButton> = {
  args: {
    variant: "outline",
    size: "sm",
    children: "小サイズ アウトライン",
  },
};

export const WithIcon: StoryObj<typeof VariantButton> = {
  args: {
    variant: "outline",
    size: "sm",
    children: (
      <>
        <FileTextIcon className="icon-sm" />
        記事
      </>
    ),
  },
};

export const Disabled: StoryObj<typeof VariantButton> = {
  args: {
    disabled: true,
    children: "無効",
  },
};

export const DisabledOutline: StoryObj<typeof VariantButton> = {
  args: {
    variant: "outline",
    disabled: true,
    children: "無効",
  },
};
