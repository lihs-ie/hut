import { edit as editAction } from "@/actions/article";
import { uploadImage } from "@/actions/common";
import { findBySlug } from "@shared/actions/article";
import { getAllTags } from "@shared/actions/tag";
import { ArticleEdit } from "@shared/components/templates/article/edit";
import {
  ArticleSnapshot,
  toSnapshot,
  UnvalidatedArticle,
} from "@shared/domains/articles";

type Props = {
  params: Promise<{ slug: string }>;
};

function edit(before: ArticleSnapshot) {
  return async (validated: UnvalidatedArticle) => {
    "use server";

    return editAction(validated, before);
  };
}

export default async function ArticleEditPage(props: Props) {
  const { slug } = await props.params;

  const [article, tags] = await Promise.all([findBySlug(slug), getAllTags()]);

  return (
    <ArticleEdit
      initial={article}
      tags={tags}
      persist={edit(toSnapshot(article))}
      uploadImage={uploadImage}
    />
  );
}
