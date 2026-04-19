import { findAllTags } from "@/actions/tag";
import { MDXRenderer } from "@shared/components/global/mdx";
import { ArticleIndex } from "@shared/components/templates/article";
import {
  createTableOfContents,
  findBySlug,
  searchAllSlugs,
} from "@/actions/article";
import { incrementViewCount } from "@/actions/view";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string }>;
};

export async function generateStaticParams() {
  const slugs = await searchAllSlugs();

  return slugs.map((slug) => ({ slug }));
}

export default async function ArticleDetailPage(props: Props) {
  const params = await props.params;

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
    />
  );
}
