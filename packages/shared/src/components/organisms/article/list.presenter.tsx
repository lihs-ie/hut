import { Article } from "@shared/domains/articles";
import styles from "./list.module.css";
import { ArticleCard } from "@shared/components/molecules/list/card/article";

export type Props = {
  articles: Article[];
};

export const ArticleListPresenter = (props: Props) => (
  <div className={styles.container}>
    {props.articles.map((article) => (
      <ArticleCard key={article.identifier} article={article} />
    ))}
  </div>
);
