import { Search } from "@/app/actions/article";
import { Criteria } from "@/domains/articles";
import { ArticleListPresenter } from "./list.presenter";

export type Props = {
  criteria: Criteria;
};

export const ArticleList = async (props: Props) => {
  const articles = await Search({ freeWord: "サンプル" });

  return <ArticleListPresenter articles={articles} />;
};
