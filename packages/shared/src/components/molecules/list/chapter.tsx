import { Chapter, ChapterSlug, SeriesIdentifier } from "@shared/domains/series";
import { ChapterTitleCard } from "./card/series/chapter-title";
import styles from "./chapter.module.css";

export type Props = {
  chapters: Chapter[];
  series: SeriesIdentifier;
  slug: ChapterSlug;
};

export const ChapterList = (props: Props) => (
  <div className={styles.container}>
    {props.chapters.map((chapter, index) => (
      <ChapterTitleCard
        key={chapter.title}
        title={chapter.title}
        index={index + 1}
        series={props.series}
        slug={props.slug}
      />
    ))}
  </div>
);
