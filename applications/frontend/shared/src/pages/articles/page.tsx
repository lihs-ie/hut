import { search } from "@shared/actions/article";
import { findAllTags } from "@shared/actions/tag";
import { ArticleListIndex } from "@shared/components/templates/article/list";

export default async function ArticleListPage() {
  return (
    <ArticleListIndex
      search={async () => await search({})}
      findAllTags={findAllTags}
    />
  );
}
