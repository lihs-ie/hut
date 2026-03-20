import { findAllTags } from "@shared/actions/tag";
import { SeriesIndex } from "@shared/components/templates/series";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug, searchAllSlugs } from "@/actions/series";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string }>;
};

export async function generateStaticParams() {
  const slugs = await searchAllSlugs();

  return slugs.map((slug) => ({ slug }));
}

export default async function SeriesDetailPage(props: Props) {
  const params = await props.params;

  return (
    <SeriesIndex
      slug={slugSchema.parse(params.slug)}
      findBySlug={findBySlug}
      findAllTags={findAllTags}
    />
  );
}
