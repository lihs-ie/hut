import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { HomeContentCard } from "@shared/components/molecules/list/card/home-content";
import { ContentType } from "@shared/domains/search-token";
import { Forger } from "@lihs-ie/forger-ts";
import { SlugMold } from "../../../../../support/molds/domains/common/slug";
import { TagNameMold } from "../../../../../support/molds/domains/attributes/tag";

const meta = {
  component: HomeContentCard,
} satisfies Meta<typeof HomeContentCard>;

export default meta;

const slug = Forger(SlugMold).forge();

export const Article: StoryObj<typeof HomeContentCard> = {
  args: {
    slug: slug,
    type: ContentType.ARTICLE,
    title: "React Server Components の実践ガイド",
    date: new Date("2024-01-15"),
    tagNames: [
      Forger(TagNameMold).forge(),
      Forger(TagNameMold).forge(),
      Forger(TagNameMold).forge(),
    ],
  },
};

export const Memo: StoryObj<typeof HomeContentCard> = {
  args: {
    slug: slug,
    type: ContentType.MEMO,
    title: "TypeScript 5.0 の新機能メモ",
    date: new Date("2024-02-20"),
    tagNames: [Forger(TagNameMold).forge()],
  },
};

export const WithoutTags: StoryObj<typeof HomeContentCard> = {
  args: {
    slug: slug,
    type: ContentType.ARTICLE,
    title: "タグなしの記事サンプル",
    date: new Date("2024-03-10"),
    tagNames: [],
  },
};

export const LongTitle: StoryObj<typeof HomeContentCard> = {
  args: {
    slug: slug,
    type: ContentType.ARTICLE,
    title: "これは非常に長いタイトルのサンプルです。タイトルが長い場合の表示をテストします。",
    date: new Date("2024-04-01"),
    tagNames: [Forger(TagNameMold).forge(), Forger(TagNameMold).forge()],
  },
};

export const GridLayout: StoryObj<typeof HomeContentCard> = {
  render: () => (
    <div style={{ display: "grid", gridTemplateColumns: "repeat(2, 1fr)", gap: "1rem" }}>
      <HomeContentCard
        slug={slug}
        type={ContentType.ARTICLE}
        title="記事タイトル 1"
        date={new Date("2024-01-15")}
        tagNames={[Forger(TagNameMold).forge()]}
      />
      <HomeContentCard
        slug={slug}
        type={ContentType.MEMO}
        title="メモタイトル 1"
        date={new Date("2024-01-20")}
        tagNames={[Forger(TagNameMold).forge(), Forger(TagNameMold).forge()]}
      />
      <HomeContentCard
        slug={slug}
        type={ContentType.ARTICLE}
        title="記事タイトル 2"
        date={new Date("2024-02-10")}
        tagNames={[]}
      />
      <HomeContentCard
        slug={slug}
        type={ContentType.MEMO}
        title="メモタイトル 2"
        date={new Date("2024-02-15")}
        tagNames={[Forger(TagNameMold).forge()]}
      />
    </div>
  ),
};
