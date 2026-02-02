import { Article } from "@shared/domains/articles";
import styles from "./list.module.css";
import { ArticleColumnCard } from "@shared/components/molecules/list/card/article/column";

export type Props = {
  articles: Article[];
};

export const ArticleListPresenter = (props: Props) => (
  <div className={styles.container}>
    {props.articles.map((article) => (
      <ArticleColumnCard key={article.identifier} article={article} />
    ))}
  </div>
);
