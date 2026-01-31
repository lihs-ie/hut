import { findBySlug } from "@shared/actions/memo";
import { findAllTags } from "@shared/actions/tag";
import { MDXRenderer } from "@shared/components/global/mdx";
import { MemoIndex } from "@shared/components/templates/memo";
import type { Metadata } from "next";

type Props = {
  params: Promise<{ slug: string }>;
};
const generateExcerpt = (entries: { text: string }[]): string => {
  if (entries.length === 0) return "";
  const firstEntry = entries[0].text;
  return firstEntry.length > 160
    ? firstEntry.slice(0, 157) + "..."
    : firstEntry;
};

export const generateMetadata = async (props: Props): Promise<Metadata> => {
  const { slug } = await props.params;
  const memo = await findBySlug(slug);
  const tags = await findAllTags(memo.tags);

  const description = generateExcerpt(memo.entries);
  // const url = `https://your-domain.com/memos/${slug}`; TODO: ドメイン取得後対応

  return {
    title: memo.title,
    description: description || `${memo.title}についてのメモです`,
    keywords: tags.map((tag) => tag.name),
    openGraph: {
      type: "article",
      title: memo.title,
      description,
      // url,
      publishedTime: memo.timeline.createdAt.toISOString(),
      modifiedTime: memo.timeline.updatedAt.toISOString(),
    },
    twitter: {
      card: "summary",
      title: memo.title,
      description,
    },
    alternates: {
      // canonical: url,
    },
  };
};

export default async function MemoDetailPage(props: Props) {
  const params = await props.params;

  return (
    <MemoIndex
      slug={params.slug}
      findAllTags={findAllTags}
      findBySlug={findBySlug}
      renderer={MDXRenderer}
    />
  );
}
