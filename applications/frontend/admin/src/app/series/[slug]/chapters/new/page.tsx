import { persist } from "@/actions/chapter";
import { uploadImage } from "@/actions/common";
import { ChapterEdit } from "@shared/components/templates/series/chapter/edit";
import { UnvalidatedChapter } from "@shared/domains/series/chapter";

type Props = {
  params: Promise<{ slug: string }>;
};

export default async function ChapterCreatePage(props: Props) {
  const { slug } = await props.params;

  async function createChapter(unvalidated: UnvalidatedChapter) {
    "use server";
    await persist(unvalidated, slug);
  }

  return (
    <ChapterEdit
      persist={createChapter}
      uploadImage={uploadImage}
      seriesSlug={slug}
    />
  );
}
