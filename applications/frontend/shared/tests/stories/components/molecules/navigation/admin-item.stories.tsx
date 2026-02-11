import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { FileTextIcon } from "@shared/components/atoms/icon/file-text";
import { MessageSquareIcon } from "@shared/components/atoms/icon/message";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { InfoIcon } from "@shared/components/atoms/icon/info";

import { AdminNavItem } from "@shared/components/molecules/navigation/admin-item";

const meta = {
  component: AdminNavItem,
} satisfies Meta<typeof AdminNavItem>;

export default meta;

export const Default: StoryObj<typeof AdminNavItem> = {
  args: {
    href: "/admin/articles",
    icon: <FileTextIcon className="icon-md" />,
    label: "記事の管理",
    isActive: false,
  },
};

export const Active: StoryObj<typeof AdminNavItem> = {
  args: {
    href: "/admin/articles",
    icon: <FileTextIcon className="icon-md" />,
    label: "記事の管理",
    isActive: true,
  },
};

export const Scraps: StoryObj<typeof AdminNavItem> = {
  args: {
    href: "/admin/scraps",
    icon: <MessageSquareIcon className="icon-md" />,
    label: "スクラップ",
    isActive: false,
  },
};

export const Books: StoryObj<typeof AdminNavItem> = {
  args: {
    href: "/admin/books",
    icon: <BookOpenIcon className="icon-md" />,
    label: "本の管理",
    isActive: false,
  },
};

export const Stats: StoryObj<typeof AdminNavItem> = {
  args: {
    href: "/admin/stats",
    icon: <InfoIcon className="icon-md" />,
    label: "統計",
    isActive: true,
  },
};
