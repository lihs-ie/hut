import { searchArticles } from "@/actions/feed/article-search";
import { findAllTags } from "@/actions/tag";
import { ArticleListIndex } from "@shared/components/templates/article/list";

export const revalidate = 60;

/** Lists published Articles from the Reader's active Article source. */
export default function Page() {
  return <ArticleListIndex search={() => searchArticles({})} findAllTags={findAllTags} />;
}
