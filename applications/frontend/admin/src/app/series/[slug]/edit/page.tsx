import { findBySlug, persist as persistAction } from "@/actions/series";
import { terminate as terminateChapterAction } from "@/actions/chapter";
import { getAllTags } from "@/actions/tag";
import { findChaptersByIdentifiers } from "@/actions/chapter";
import { SeriesEdit } from "@shared/components/templates/series/edit";
import { UnvalidatedSeries } from "@shared/domains/series";
import { slugSchema } from "@shared/domains/common/slug";

type Props = {
  params: Promise<{ slug: string }>;
};

function edit(seriesSlug: string) {
  return async (unvalidated: UnvalidatedSeries) => {
    "use server";

    return persistAction({ ...unvalidated, slug: seriesSlug });
  };
}

async function terminateChapter(chapterIdentifier: string, seriesSlug: string) {
  "use server";

  return terminateChapterAction(chapterIdentifier, seriesSlug);
}

export default async function SeriesEditPage(props: Props) {
  const { slug } = await props.params;

  const [series, tags] = await Promise.all([findBySlug(slug), getAllTags()]);
  const chapters = await findChaptersByIdentifiers(series.chapters);

  return (
    <SeriesEdit
      initial={series}
      tags={tags}
      persist={edit(slug)}
      chapters={chapters}
      seriesSlug={slugSchema.parse(slug)}
      terminateChapter={terminateChapter}
    />
  );
}
