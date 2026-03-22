import { findBySlug, persist as persistAction } from "@/actions/chapter";
import { uploadImage } from "@/actions/common";
import { ChapterEdit } from "@shared/components/templates/series/chapter/edit";
import { UnvalidatedChapter } from "@shared/domains/series/chapter";

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

function edit(chapterSlug: string) {
  return async (unvalidated: UnvalidatedChapter) => {
    "use server";

    return persistAction({ ...unvalidated, slug: chapterSlug });
  };
}

export default async function ChapterEditPage(props: Props) {
  const { slug, chapter } = await props.params;

  const chapterData = await findBySlug(chapter);

  return (
    <ChapterEdit
      initial={chapterData}
      persist={edit(chapter)}
      uploadImage={uploadImage}
      seriesSlug={slug}
    />
  );
}
