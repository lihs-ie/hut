import Link from "next/link";
import Image from "next/image";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { SimpleBadge } from "@shared/components/atoms/badge/simple";
import styles from "./summary.module.css";

export type Props = {
  slug: string;
  title: string;
  description?: string;
  cover?: string | null;
  emoji?: string;
  tags: string[];
  chapterCount: number;
};

export const SeriesSummaryCard = (props: Props) => {
  return (
    <Link href={`/series/${props.slug}`} className={styles.container}>
      <div className={styles.header}>
        <div className={styles.cover}>
          {props.cover ? (
            <Image
              src={props.cover}
              alt={props.title}
              width={64}
              height={64}
              className={styles["cover-image"]}
            />
          ) : (
            props.emoji || "📚"
          )}
        </div>
      </div>
      <h2 className={styles.title}>{props.title}</h2>
      {props.description && (
        <p className={styles.description}>{props.description}</p>
      )}
      <div className={styles.tags}>
        {props.tags.slice(0, 3).map((tag) => (
          <SimpleBadge key={tag} label={tag} />
        ))}
      </div>
      <div className={styles.footer}>
        <div className={styles.stats}>
          <div className={styles.stat}>
            <BookOpenIcon className={styles["stat-icon"]} />
            <span>{props.chapterCount}</span>
          </div>
        </div>
      </div>
    </Link>
  );
};
