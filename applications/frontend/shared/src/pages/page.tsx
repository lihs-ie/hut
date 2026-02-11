import { getProfile } from "@shared/actions/admin";
import { search as searchArticles } from "@shared/actions/article";
import { search as searchMemos } from "@shared/actions/memo";
// [初期リリース対象外] import { search as searchSeries } from "@shared/actions/series";
import { findAllTags } from "@shared/actions/tag";
import { TopIndex } from "@shared/components/templates/top";
import { PublishStatus } from "@shared/domains/common";

export default async function TopPage() {
  return (
    <TopIndex
      searchArticles={async () => searchArticles({})}
      searchMemos={async () =>
        searchMemos({
          freeWord: null,
          tags: null,
          status: PublishStatus.PUBLISHED,
        })
      }
      // [初期リリース対象外] searchSeries={async () => searchSeries({ slug: null, tags: null })}
      findAllTags={findAllTags}
      getProfile={getProfile}
    />
  );
}
