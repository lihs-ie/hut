import { getProfile } from "@shared/actions/admin";
import { search as searchArticles } from "@/actions/article";
import { search as searchMemos } from "@/actions/memo";
import { search as searchSeries } from "@/actions/series";
import { findAllTags } from "@/actions/tag";
import { TopIndex } from "@shared/components/templates/top";

export default async function TopPage() {
  return (
    <TopIndex
      searchArticles={async () => searchArticles({})}
      searchMemos={async () =>
        searchMemos({
          freeWord: null,
          tags: null,
          status: null,
        })
      }
      searchSeries={async () => searchSeries({ slug: null, tags: null, status: null, freeWord: null })}
      findAllTags={findAllTags}
      getProfile={getProfile}
    />
  );
}
