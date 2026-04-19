import { findBySlug, searchAllSlugs } from "@/actions/memo";
import { findAllTags } from "@/actions/tag";
import { MDXRenderer } from "@shared/components/global/mdx";
import { MemoIndex } from "@shared/components/templates/memo";
import { incrementViewCount } from "@/actions/view";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string }>;
};

export async function generateStaticParams() {
  const slugs = await searchAllSlugs();

  return slugs.map((slug) => ({ slug }));
}

export default async function MemoDetailPage(props: Props) {
  const params = await props.params;

  await findBySlug(params.slug);

  return (
    <MemoIndex
      slug={params.slug}
      findAllTags={findAllTags}
      findBySlug={findBySlug}
      renderer={MDXRenderer}
      incrementViewCount={incrementViewCount}
    />
  );
}
