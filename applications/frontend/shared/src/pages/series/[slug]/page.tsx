import { findAllTags } from "@shared/actions/tag";
import { SeriesIndex } from "@shared/components/templates/series";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@shared/actions/series";

type Props = {
  params: Promise<{ slug: string }>;
};

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
