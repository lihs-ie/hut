import { ChapterSlug, SeriesIdentifier } from "@shared/domains/series";
import Link from "next/link";
import styles from "./chapter-title.module.css";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";

export type Props = {
  title: string;
  slug: ChapterSlug;
  series: SeriesIdentifier;
  index: number;
};

export const ChapterTitleCard = (props: Props) => (
  <Link
    className={styles.container}
    href={`/series/${props.series}/chapters/${props.slug}`}
  >
    <div className={styles.contents}>
      <div className={styles.chapterNumber}>{props.index}</div>
      <h3 className={styles.chapterTitle}>{props.title}</h3>
    </div>
    <ChevronRightIcon className={styles.right} />
  </Link>
);
