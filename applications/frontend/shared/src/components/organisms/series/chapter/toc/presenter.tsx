import { SeriesSlug } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { ChapterTOCLink } from "@shared/components/molecules/link/chapter-toc-link";
import { NavigableLink } from "@shared/components/molecules/link/navigable";
import { Routes } from "@shared/config/presentation/route";
import styles from "./presenter.module.css";

export type Props = {
  slug: SeriesSlug;
  seriesTitle: string;
  chapters: Chapter[];
};

export const ChapterTOCPresenter = (props: Props) => (
  <aside className={styles.container}>
    <div className={styles.inner}>
      <NavigableLink
        href={Routes.page.series.show(props.slug)}
        className={styles.link}
      >
        <BookOpenIcon className={styles.icon} />
        {props.seriesTitle}
      </NavigableLink>

      <nav className={styles.list} aria-label="チャプター一覧">
        {props.chapters.map((chapter, index) => (
          <ChapterTOCLink
            key={chapter.slug}
            href={Routes.page.series.chapter.show(props.slug, chapter.slug)}
            className={styles.item}
            activeClassName={`${styles.item} ${styles.active}`}
          >
            <span className={styles.number}>
              {String(index + 1).padStart(2, "0")}
            </span>
            <span className={styles.title}>{chapter.title}</span>
          </ChapterTOCLink>
        ))}
      </nav>
    </div>
  </aside>
);
