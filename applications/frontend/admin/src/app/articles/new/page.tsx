import { create } from "@/actions/article";
import { uploadImage } from "@/actions/common";
import { getAllTags } from "@shared/actions/tag";
import { ArticleEdit } from "@shared/components/templates/article/edit";

export default async function ArticleCreatePage() {
  const tags = await getAllTags();

  return <ArticleEdit tags={tags} persist={create} uploadImage={uploadImage} />;
}
