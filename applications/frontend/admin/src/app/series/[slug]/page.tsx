import { findAllTags } from "@/actions/tag";
import { findChaptersByIdentifiers } from "@/actions/chapter";
import { SeriesIndex } from "@shared/components/templates/series";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@/actions/series";

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
