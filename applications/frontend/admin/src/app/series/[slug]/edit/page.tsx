import { findBySlug, persist as persistAction } from "@/actions/series";
import { getAllTags } from "@shared/actions/tag";
import { SeriesEdit } from "@shared/components/templates/series/edit";
import { UnvalidatedSeries } from "@shared/domains/series";

type Props = {
  params: Promise<{ slug: string }>;
};

function edit(seriesSlug: string) {
  return async (unvalidated: UnvalidatedSeries) => {
    "use server";

    return persistAction({ ...unvalidated, slug: seriesSlug });
  };
}

export default async function SeriesEditPage(props: Props) {
  const { slug } = await props.params;

  const [series, tags] = await Promise.all([findBySlug(slug), getAllTags()]);

  return (
    <SeriesEdit
      initial={series}
      tags={tags}
      persist={edit(slug)}
    />
  );
}
