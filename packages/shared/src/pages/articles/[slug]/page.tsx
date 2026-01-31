import { findAllTags } from "@shared/actions/tag";
import { MDXRenderer } from "@shared/components/global/mdx";
import { ArticleIndex } from "@shared/components/templates/article";
import { createTableOfContents, findBySlug } from "@shared/actions/article";

type Props = {
  params: Promise<{ slug: string }>;
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
