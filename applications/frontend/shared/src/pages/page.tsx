import { getProfile } from "@shared/actions/admin";
import { search as searchArticles } from "@shared/actions/article";
import { findAllTags } from "@shared/actions/tag";
import { TopIndex } from "@shared/components/templates/top";

export default async function TopPage() {
  return (
    <TopIndex
      searchArticles={async () => searchArticles({})}
      findAllTags={findAllTags}
      getProfile={getProfile}
    />
  );
}
