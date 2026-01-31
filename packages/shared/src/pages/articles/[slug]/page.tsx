import { findAllTags } from "@shared/actions/tag";
import { MDXRenderer } from "@shared/components/global/mdx";
import { ArticleIndex } from "@shared/components/templates/article";
import { createTableOfContents, findBySlug } from "@shared/actions/article";
import { Metadata } from "next/dist/types";

type Props = {
  params: Promise<{ slug: string }>;
};

export const generateMetadata = async (props: Props): Promise<Metadata> => {
  const { slug } = await props.params;
  const article = await findBySlug(slug);
  const tags = await findAllTags(article.tags);

  // const url = `https://hut.li/articles/${article.slug}`; TODO: ドメイン取得後対応

  return {
    title: article.title,
    description: article.excerpt,
    keywords: tags.map((tag) => tag.name),
    authors: [{ name: "lihs" }],
    openGraph: {
      type: "article",
      title: article.title,
      description: article.excerpt,
      // url,
      publishedTime: article.timeline.createdAt.toISOString(),
      modifiedTime: article.timeline.updatedAt.toISOString(),
      tags: tags.map((tag) => tag.name),
    },
    twitter: {
      card: "summary_large_image",
      title: article.title,
      description: article.excerpt,
    },
    alternates: {
      // canonical: url,
    },
  };
};

export default async function ArticleDetailPage(props: Props) {
  const params = await props.params;

  return (
    <ArticleIndex
      slug={params.slug}
      article={{
        findAllTags,
        findBySlug,
        renderer: MDXRenderer,
      }}
      sidebar={{
        createTableOfContents,
      }}
    />
  );
}
