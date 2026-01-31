import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import {
  FileTextIcon,
  MessageSquareIcon,
  BookOpenIcon,
  InfoIcon,
  GithubIcon,
} from "@shared/components/atoms/icon";

import { AdminSidebar } from "../../../../../../admin/src/app/admin/_components/organisms/admin/sidebar";

const meta = {
  component: AdminSidebar,
  parameters: {
    layout: "fullscreen",
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
    currentPath: "/admin/scraps",
  },
};

export const ArticlesActive: StoryObj<typeof AdminSidebar> = {
  args: {
    items: defaultItems,
    currentPath: "/admin/articles",
  },
};

export const BooksActive: StoryObj<typeof AdminSidebar> = {
  args: {
    items: defaultItems,
    currentPath: "/admin/books",
  },
};

export const StatsActive: StoryObj<typeof AdminSidebar> = {
  args: {
    items: defaultItems,
    currentPath: "/admin/stats",
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
    currentPath: "/admin/articles",
  },
};
