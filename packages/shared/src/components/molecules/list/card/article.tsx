import { Article } from "@/domains/articles";
import styles from "./article.module.css";
import Image from "next/image";

export type Props = {
  article: Article;
};

export const ArticleCard = (props: Props) => (
  <div className={styles.container}>
    <h2 className={styles.title}>{props.article.title}</h2>
    <p>{props.article.excerpt}</p>
  </div>
);
