import { findAllTags } from "@shared/actions/tag";
import { findChaptersByIdentifiers } from "@shared/actions/chapter";
import { SeriesIndex } from "@shared/components/templates/series";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@shared/actions/series";

type Props = {
  params: Promise<{ slug: string }>;
};

export default async function AdminSeriesDetailPage(props: Props) {
  const params = await props.params;

  return (
    <SeriesIndex
      slug={slugSchema.parse(params.slug)}
      findBySlug={findBySlug}
      findChaptersByIdentifiers={findChaptersByIdentifiers}
      findAllTags={findAllTags}
    />
  );
}
