import { findBySlug } from "@/actions/memo";
import { findAllTags } from "@shared/actions/tag";
import { MDXRenderer } from "@shared/components/global/mdx";
import { MemoIndex } from "@shared/components/templates/memo";
import { incrementViewCount } from "@shared/actions/view";

type Props = {
  params: Promise<{ slug: string }>;
};

export default async function MemoDetailPage(props: Props) {
  const params = await props.params;

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
