import Link from "next/link";
import { Series, SeriesSlug } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { Tag } from "@shared/domains/attributes/tag";
import { TagBadgeList } from "@shared/components/organisms/common/list/tag";
import { Routes } from "@shared/config/presentation/route";
import styles from "./index.module.css";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";

export type Props = {
  series: Series;
  slug: SeriesSlug;
  chapters: Chapter[];
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const SeriesDetail = (props: Props) => {
  const series = props.series;

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <h1 className={styles["main-title"]}>{series.title}</h1>
        {series.subTitle && (
          <p className={styles["main-subtitle"]}>{series.subTitle}</p>
        )}

        <div className={styles["topic-list"]}>
          <TagBadgeList identifiers={series.tags} findAllTags={props.findAllTags} />
        </div>

        {series.description && (
          <p className={styles.description}>{series.description}</p>
        )}
      </div>

      <section className={styles["chapters-section"]} aria-label="目次">
        <h2 className={styles["chapters-title"]}>目次</h2>
        <ol className={styles["chapter-list"]}>
          {props.chapters.map((chapter, index) => (
            <li key={chapter.slug}>
              <Link
                href={Routes.page.series.chapter.show(props.slug, chapter.slug)}
                className={styles["chapter-card"]}
              >
                <div className={styles["chapter-content"]}>
                  <div className={styles["chapter-info"]}>
                    <span className={styles["chapter-number"]}>{String(index + 1).padStart(2, "0")}</span>
                    <h3 className={styles["chapter-title"]}>{chapter.title}</h3>
                  </div>
                  <ChevronRightIcon className={styles["chapter-icon"]} />
                </div>
              </Link>
            </li>
          ))}
        </ol>
      </section>
    </div>
  );
};
