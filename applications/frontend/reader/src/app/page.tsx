import { getProfile } from "@/actions/admin";
import { searchArticles, searchMemos, searchSeries } from "@/actions/feed/article-search";
import { findAllTags } from "@/actions/tag";
import { TopIndex } from "@shared/components/templates/top";
import { PublishStatus } from "@shared/domains/common";

export const revalidate = 60;

/** Renders the reader home page from the active Article, Memo, and Series sources. */
export default function Page() {
  return (
    <TopIndex
      searchArticles={() => searchArticles({})}
      searchMemos={() => searchMemos({
        freeWord: null,
        tags: null,
        status: PublishStatus.PUBLISHED,
      })}
      searchSeries={searchSeries}
      findAllTags={findAllTags}
      getProfile={getProfile}
    />
  );
}
