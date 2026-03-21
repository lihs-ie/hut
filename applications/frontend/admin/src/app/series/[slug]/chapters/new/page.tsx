import { persist } from "@/actions/chapter";
import { uploadImage } from "@/actions/common";
import { ChapterEdit } from "@shared/components/templates/series/chapter/edit";

type Props = {
  params: Promise<{ slug: string }>;
};

export default async function ChapterCreatePage(props: Props) {
  const { slug } = await props.params;

  return (
    <ChapterEdit
      persist={persist}
      uploadImage={uploadImage}
      seriesSlug={slug}
    />
  );
}
