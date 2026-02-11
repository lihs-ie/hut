import { Routes } from "@shared/config/presentation/route";
import { Article } from "@shared/domains/articles";
import Link from "next/link";
import styles from "./column.module.css";
import { CommonImage } from "@shared/components/atoms/image/common";
import { SimpleBadge } from "@shared/components/atoms/badge/simple";
import { formatDate } from "@shared/aspects/date";

export type Props = {
  article: Article;
};

export const ArticleColumnCard = (props: Props) => (
  <Link
    className={styles.container}
    href={Routes.page.articles.show(props.article.slug)}
  >
    <div className={styles.image}>
      <CommonImage src={props.article.image} alt={props.article.title} />
    </div>
    <div className={styles.body}>
      <div className={styles.content}>
        <div className={styles.tags}>
          {props.article.tags.map((tag) => (
            <SimpleBadge key={tag} label={tag} />
          ))}
        </div>
        <h3 className={styles.title}>{props.article.title}</h3>
        <p className={styles.excerpt}>{props.article.excerpt}</p>
      </div>

      <div className={styles.footer}>
        <span className={styles.createdAt}>
          投稿日時: {formatDate(props.article.timeline.createdAt)}
        </span>
        <span className={styles.updatedAt}>
          最終更新日時: {formatDate(props.article.timeline.updatedAt)}
        </span>
      </div>
    </div>
  </Link>
);
