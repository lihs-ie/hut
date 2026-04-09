import { persist } from "@/actions/series";
import { getAllTags } from "@/actions/tag";
import { uploadImage } from "@/actions/common";
import { SeriesEdit } from "@shared/components/templates/series/edit";

export default async function SeriesCreatePage() {
  const tags = await getAllTags();

  return <SeriesEdit tags={tags} persist={persist} uploadImage={uploadImage} />;
}
