import { Article } from "@/domains/articles";
import { ReactNode } from "react";
import styles from "./index.module.css";

export type Props = {
  article: Article;
  renderer: (article: Article) => ReactNode;
};

export const ArticlePresenter = (props: Props) => (
  <article className={styles.container}>
    {props.renderer(props.article)}
  </article>
);
