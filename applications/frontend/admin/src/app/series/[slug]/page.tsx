import { findBySlug } from "@shared/actions/series";
import { SeriesIndex } from "@shared/components/templates/series";

type Props = {
  params: Promise<{ slug: string }>;
};

export default async function SeriesDetailPage(props: Props) {
  const params = await props.params;

  return (
    <SeriesIndex
      slug={params.slug}
      findBySlug={findBySlug}
    />
  );
}
