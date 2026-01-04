import { Find } from "@/app/actions/article";
import { ReactNode } from "react";
import { Article } from "@/domains/articles";
import { ArticlePresenter } from "./index.presenter";

export type Props = {
  slug: string;
  renderer: (article: Article) => ReactNode;
};

export const ArticleIndex = async (props: Props) => {
  const article = await Find(props.slug);

  return <ArticlePresenter article={article} renderer={props.renderer} />;
};
