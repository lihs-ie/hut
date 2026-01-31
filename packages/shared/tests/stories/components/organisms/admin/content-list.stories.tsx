import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { LockIcon } from "@shared/components/atoms/icon/lock";
import { GlobeIcon } from "@shared/components/atoms/icon/globe";
import { MessageSquareIcon } from "@shared/components/atoms/icon/message";

import { AdminContentList } from "../../../../../../admin/src/app/admin/_components/organisms/admin/content-list";
import { AdminItemCard } from "@shared/components/molecules/list/card/admin/item";

const meta = {
  component: AdminContentList,
} satisfies Meta<typeof AdminContentList>;

export default meta;

const statusOptions = [
  { value: "all", label: "すべて" },
  { value: "public", label: "公開" },
  { value: "private", label: "限定公開" },
];

const sortOptions = [
  { value: "latest", label: "最新" },
  { value: "oldest", label: "古い順" },
  { value: "comments", label: "コメント数" },
];

export const Default: StoryObj<typeof AdminContentList> = {
  args: {
    title: "スクラップの管理",
    createHref: "/admin/scraps/new",
    statusOptions,
    sortOptions,
    children: (
      <>
        <AdminItemCard
          href="/admin/scraps/1/edit"
          title="スクラップテスト"
          badges={[
            { type: "closed", label: "Closed" },
            {
              type: "visibility",
              label: "限定公開",
              icon: <LockIcon className="icon-sm" />,
            },
          ]}
          meta={[
            { label: "17時間前にクローズ" },
            { label: "2", icon: <MessageSquareIcon className="icon-sm" /> },
          ]}
        />
        <AdminItemCard
          href="/admin/scraps/2/edit"
          title="React Server Componentsの実装パターン"
          badges={[
            { type: "open", label: "Open" },
            {
              type: "visibility",
              label: "公開",
              icon: <GlobeIcon className="icon-sm" />,
            },
          ]}
          meta={[
            { label: "3日前に更新" },
            { label: "8", icon: <MessageSquareIcon className="icon-sm" /> },
          ]}
        />
      </>
    ),
  },
};

export const Articles: StoryObj<typeof AdminContentList> = {
  args: {
    title: "記事の管理",
    createHref: "/admin/articles/new",
    createLabel: "新規記事",
    searchPlaceholder: "記事を検索",
    statusOptions: [
      { value: "all", label: "すべて" },
      { value: "published", label: "公開中" },
      { value: "draft", label: "下書き" },
    ],
    sortOptions,
    children: (
      <>
        <AdminItemCard
          href="/admin/articles/1/edit"
          title="Next.js 15の新機能について"
          badges={[{ type: "open", label: "公開中" }]}
          meta={[{ label: "2024-01-14" }]}
        />
        <AdminItemCard
          href="/admin/articles/2/edit"
          title="TypeScriptの型推論を深掘りする"
          badges={[{ type: "closed", label: "下書き" }]}
          meta={[{ label: "2024-01-13" }]}
        />
      </>
    ),
  },
};

export const Empty: StoryObj<typeof AdminContentList> = {
  args: {
    title: "本の管理",
    createHref: "/admin/books/new",
    statusOptions,
    sortOptions,
    children: (
      <div
        style={{
          padding: "2rem",
          textAlign: "center",
          color: "var(--muted-foreground)",
        }}
      >
        コンテンツがありません
      </div>
    ),
  },
};

export const ManyItems: StoryObj<typeof AdminContentList> = {
  args: {
    title: "スクラップの管理",
    createHref: "/admin/scraps/new",
    statusOptions,
    sortOptions,
    children: (
      <>
        {[1, 2, 3, 4, 5].map((index) => (
          <AdminItemCard
            key={index}
            href={`/admin/scraps/${index}/edit`}
            title={`スクラップ ${index}`}
            badges={[
              {
                type: index % 2 === 0 ? "open" : "closed",
                label: index % 2 === 0 ? "Open" : "Closed",
              },
            ]}
            meta={[{ label: `${index}時間前` }]}
          />
        ))}
      </>
    ),
  },
};
