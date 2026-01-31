// import { Search } from "@shared/app/actions/article";
import { Criteria } from "@shared/domains/articles";
import { ArticleListPresenter } from "./list.presenter";

export type Props = {
  criteria: Criteria;
};

export const ArticleList = async (_: Props) => {
  // const articles = await Search({ freeWord: "サンプル" });

  return <ArticleListPresenter articles={[]} />;
};
