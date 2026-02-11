import { Chapter, SeriesIdentifier } from "@shared/domains/series";
import styles from "./chapter.module.css";
import { ChapterTitleCard } from "@shared/components/molecules/list/card/series/chapter-title";

export type Props = {
  chapters: Chapter[];
  slug: string;
  series: SeriesIdentifier;
};

export const SeriesChapters = (props: Props) => (
  <div className={styles["chapters-section"]}>
    <h2 className={styles["chapters-title"]}>目次</h2>
    <div className={styles["chapter-list"]}>
      {props.chapters.map((chapter, index) => (
        <ChapterTitleCard
          key={chapter.slug}
          title={chapter.title}
          slug={chapter.slug}
          series={props.series}
          index={index + 1}
        />
      ))}
    </div>
  </div>
);
