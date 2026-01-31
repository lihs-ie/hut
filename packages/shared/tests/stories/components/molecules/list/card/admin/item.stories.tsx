import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { LockIcon, GlobeIcon, MessageSquareIcon } from "@shared/components/atoms/icon";

import { AdminItemCard } from "@shared/components/molecules/list/card/admin/item";

const meta = {
  component: AdminItemCard,
} satisfies Meta<typeof AdminItemCard>;

export default meta;

export const Default: StoryObj<typeof AdminItemCard> = {
  args: {
    href: "/admin/articles/1/edit",
    title: "Next.js 15の新機能について",
  },
};

export const WithBadges: StoryObj<typeof AdminItemCard> = {
  args: {
    href: "/admin/scraps/1/edit",
    title: "スクラップテスト",
    badges: [
      { type: "closed", label: "Closed" },
      { type: "visibility", label: "限定公開", icon: <LockIcon className="icon-sm" /> },
    ],
  },
};

export const OpenStatus: StoryObj<typeof AdminItemCard> = {
  args: {
    href: "/admin/scraps/2/edit",
    title: "公開中のスクラップ",
    badges: [
      { type: "open", label: "Open" },
      { type: "visibility", label: "公開", icon: <GlobeIcon className="icon-sm" /> },
    ],
  },
};

export const WithMeta: StoryObj<typeof AdminItemCard> = {
  args: {
    href: "/admin/scraps/1/edit",
    title: "スクラップテスト",
    badges: [
      { type: "closed", label: "Closed" },
      { type: "visibility", label: "限定公開", icon: <LockIcon className="icon-sm" /> },
    ],
    meta: [
      { label: "17時間前にクローズ" },
      { label: "2", icon: <MessageSquareIcon className="icon-sm" /> },
    ],
  },
};

export const FullExample: StoryObj<typeof AdminItemCard> = {
  args: {
    href: "/admin/articles/1/edit",
    title: "TypeScriptの型推論を深掘りする",
    badges: [
      { type: "open", label: "公開中" },
      { type: "visibility", label: "公開", icon: <GlobeIcon className="icon-sm" /> },
    ],
    meta: [
      { label: "2024-01-14 10:30" },
      { label: "5", icon: <MessageSquareIcon className="icon-sm" /> },
    ],
  },
};

export const LongTitle: StoryObj<typeof AdminItemCard> = {
  args: {
    href: "/admin/articles/1/edit",
    title:
      "これは非常に長いタイトルの記事です。タイトルが長すぎる場合にどのように表示されるかをテストしています。",
    badges: [{ type: "open", label: "Open" }],
    meta: [{ label: "1時間前" }],
  },
};
