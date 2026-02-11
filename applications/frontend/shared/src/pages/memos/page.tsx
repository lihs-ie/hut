import { search } from "@shared/actions/memo";
import { findAllTags } from "@shared/actions/tag";
import { MemoListIndex } from "@shared/components/templates/memo/list";

export default async function ArticleListPage() {
  return (
    <MemoListIndex
      search={async () =>
        await search({ tags: null, freeWord: null, status: null })
      }
      findAllTags={findAllTags}
    />
  );
}
