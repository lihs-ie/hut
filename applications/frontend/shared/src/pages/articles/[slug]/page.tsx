import { findAllTags } from "@shared/actions/tag";
import { MDXRenderer } from "@shared/components/global/mdx";
import { ArticleIndex } from "@shared/components/templates/article";
import { createTableOfContents, findBySlug } from "@shared/actions/article";
import { incrementViewCount } from "@shared/actions/view";
import { ContentType } from "@shared/domains/search-token/reference";
import type { Metadata } from "next";

type Props = {
  params: Promise<{ slug: string }>;
};

export const generateMetadata = async (props: Props): Promise<Metadata> => {
  const { slug } = await props.params;
  const article = await findBySlug(slug);
  const tags = await findAllTags(article.tags);

  return {
    title: article.title,
    description: article.excerpt,
    keywords: tags.map((tag) => tag.name),
    authors: [{ name: "lihs" }],
    openGraph: {
      type: "article",
      title: article.title,
      description: article.excerpt,
      publishedTime: article.timeline.createdAt.toISOString(),
      modifiedTime: article.timeline.updatedAt.toISOString(),
      tags: tags.map((tag) => tag.name),
    },
    twitter: {
      card: "summary_large_image",
      title: article.title,
      description: article.excerpt,
    },
  };
};

export default async function ArticleDetailPage(props: Props) {
  const params = await props.params;
  const article = await findBySlug(params.slug);

  return (
    <ArticleIndex
      slug={params.slug}
      article={{
        findAllTags,
        findBySlug,
        renderer: MDXRenderer,
        incrementViewCount,
      }}
      sidebar={{
        createTableOfContents,
      }}
      tracking={{
        contentType: ContentType.ARTICLE,
        contentIdentifier: article.identifier,
      }}
    />
  );
}
