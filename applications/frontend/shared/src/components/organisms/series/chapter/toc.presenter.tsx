import { SeriesSlug } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { ChapterTocLink } from "@shared/components/molecules/link/chapter-toc-link";
import { NavigableLink } from "@shared/components/molecules/link/navigable";
import { Routes } from "@shared/config/presentation/route";
import styles from "./toc.module.css";

export type Props = {
  slug: SeriesSlug;
  seriesTitle: string;
  allChapters: Chapter[];
};

export const ChapterTocPresenter = (props: Props) => (
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
        {props.allChapters.map((chapter, index) => (
          <ChapterTocLink
            key={chapter.slug}
            href={Routes.page.series.chapter.show(props.slug, chapter.slug)}
            className={styles.item}
            activeClassName={`${styles.item} ${styles.active}`}
          >
            <span className={styles.number}>
              {String(index + 1).padStart(2, "0")}
            </span>
            <span className={styles.title}>{chapter.title}</span>
          </ChapterTocLink>
        ))}
      </nav>
    </div>
  </aside>
);
