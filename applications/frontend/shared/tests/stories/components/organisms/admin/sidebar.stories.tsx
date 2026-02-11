import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { FileTextIcon } from "@shared/components/atoms/icon/file-text";
import { MessageSquareIcon } from "@shared/components/atoms/icon/message";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { InfoIcon } from "@shared/components/atoms/icon/info";
import { GithubIcon } from "@shared/components/atoms/icon/github";

import { AdminSidebar } from "../../../../../../admin/src/app/admin/_components/organisms/admin/sidebar";

const meta = {
  component: AdminSidebar,
  parameters: {
    layout: "fullscreen",
    nextjs: { appDirectory: true },
  },
} satisfies Meta<typeof AdminSidebar>;

export default meta;

const defaultItems = [
  {
    href: "/admin/scraps",
    icon: <MessageSquareIcon className="icon-md" />,
    label: "スクラップ",
  },
  {
    href: "/admin/articles",
    icon: <FileTextIcon className="icon-md" />,
    label: "記事の管理",
  },
  {
    href: "/admin/books",
    icon: <BookOpenIcon className="icon-md" />,
    label: "本の管理",
  },
  {
    href: "/admin/github",
    icon: <GithubIcon className="icon-md" />,
    label: "GitHub連携",
  },
  {
    href: "/admin/stats",
    icon: <InfoIcon className="icon-md" />,
    label: "統計",
  },
];

export const Default: StoryObj<typeof AdminSidebar> = {
  args: {
    items: defaultItems,
  },
};

export const MinimalItems: StoryObj<typeof AdminSidebar> = {
  args: {
    items: [
      {
        href: "/admin/articles",
        icon: <FileTextIcon className="icon-md" />,
        label: "記事",
      },
      {
        href: "/admin/stats",
        icon: <InfoIcon className="icon-md" />,
        label: "統計",
      },
    ],
  },
};
