import { getProfile } from "@/actions/admin";
import { latestArticles } from "@/actions/feed/article-search";
import { findAllTags } from "@/actions/tag";
import { TopIndex } from "@shared/components/templates/top";
import { connection } from "next/server";

/** Renders the reader home page from published articles. */
export default async function Page() {
  await connection();
  return (
    <TopIndex
      searchArticles={latestArticles}
      findAllTags={findAllTags}
      getProfile={getProfile}
    />
  );
}
