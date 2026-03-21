import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ChapterSlug, SeriesSlug } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { ChevronLeftIcon } from "@shared/components/atoms/icon/chevron-left";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";
import Link from "next/link";
import { Routes } from "@shared/config/presentation/route";
import styles from "./index.module.css";

export type Props = {
  seriesTitle: string;
  seriesSlug: SeriesSlug;
  chapterSlug: ChapterSlug;
  chapters: Chapter[];
  renderer: MarkdownRenderer;
};

export const ChapterPresenter = (props: Props) => {
  const currentChapter = props.chapters.find(
    (chapter) => chapter.slug === props.chapterSlug
  );
  const currentIndex = props.chapters.findIndex(
    (chapter) => chapter.slug === props.chapterSlug
  );

  if (!currentChapter) {
    return <div>Chapter not found</div>;
  }

  const previousChapter =
    currentIndex > 0 ? props.chapters[currentIndex - 1] : null;
  const nextChapter =
    currentIndex < props.chapters.length - 1
      ? props.chapters[currentIndex + 1]
      : null;

  return (
    <div className={styles.container}>
      <div className={styles.wrapper}>
        <div className={styles.content}>
          <Sidebar
            seriesTitle={props.seriesTitle}
            seriesSlug={props.seriesSlug}
            chapters={props.chapters}
            currentChapterSlug={props.chapterSlug}
          />
          <main className={styles.main}>
            <article>
              <div className={styles.header}>
                <p className={styles["chapter-label"]}>
                  Chapter {currentIndex + 1}
                </p>
                <h1 className={styles["chapter-heading"]}>
                  {currentChapter.title}
                </h1>
              </div>

              <div className={styles.article}>
                <div className={`prose ${styles["article-content"]}`}>
                  {props.renderer(currentChapter.content)}
                </div>
              </div>

              <BidirectionalNavigation
                seriesSlug={props.seriesSlug}
                previousChapter={previousChapter}
                nextChapter={nextChapter}
              />
            </article>
          </main>
        </div>
      </div>
    </div>
  );
};

type SidebarProps = {
  seriesTitle: string;
  seriesSlug: SeriesSlug;
  chapters: Chapter[];
  currentChapterSlug: ChapterSlug;
};

const Sidebar = (props: SidebarProps) => (
  <aside className={styles.sidebar}>
    <div className={styles["sidebar-card"]}>
      <Link
        href={Routes.page.series.show(props.seriesSlug)}
        className={styles["series-link"]}
      >
        <BookOpenIcon className={styles["series-link-icon"]} />
        <span className={styles["series-title"]}>{props.seriesTitle}</span>
      </Link>

      <nav className={styles["chapter-nav"]}>
        {props.chapters.map((chapter, index) => {
          const isActive = chapter.slug === props.currentChapterSlug;
          return (
            <Link
              key={chapter.slug}
              href={Routes.page.series.chapter.show(props.seriesSlug, chapter.slug)}
              className={`${styles["chapter-nav-item"]} ${
                isActive
                  ? styles["chapter-nav-item-active"]
                  : styles["chapter-nav-item-default"]
              }`}
            >
              <span className={styles["chapter-number"]}>{index + 1}</span>
              <span className={styles["chapter-title-text"]}>
                {chapter.title}
              </span>
            </Link>
          );
        })}
      </nav>
    </div>
  </aside>
);

type BidirectionalNavigationProps = {
  seriesSlug: SeriesSlug;
  previousChapter: Chapter | null;
  nextChapter: Chapter | null;
};

const BidirectionalNavigation = (props: BidirectionalNavigationProps) => (
  <div className={styles.navigation}>
    {props.previousChapter ? (
      <Link
        href={Routes.page.series.chapter.show(props.seriesSlug, props.previousChapter.slug)}
        className={styles["nav-link"]}
      >
        <div
          className={`${styles["nav-button"]} ${styles["nav-button-prev"]}`}
        >
          <ChevronLeftIcon
            className={`${styles["nav-icon"]} ${styles["nav-icon-left"]}`}
          />
          <div className={`${styles["nav-text"]} ${styles["nav-text-prev"]}`}>
            <span className={styles["nav-label"]}>前の章</span>
            <span className={styles["nav-title"]}>
              {props.previousChapter.title}
            </span>
          </div>
        </div>
      </Link>
    ) : (
      <div className={styles["nav-placeholder"]} />
    )}

    {props.nextChapter ? (
      <Link
        href={Routes.page.series.chapter.show(props.seriesSlug, props.nextChapter.slug)}
        className={styles["nav-link"]}
      >
        <div
          className={`${styles["nav-button"]} ${styles["nav-button-next"]}`}
        >
          <div className={`${styles["nav-text"]} ${styles["nav-text-next"]}`}>
            <span className={styles["nav-label"]}>次の章</span>
            <span className={styles["nav-title"]}>
              {props.nextChapter.title}
            </span>
          </div>
          <ChevronRightIcon
            className={`${styles["nav-icon"]} ${styles["nav-icon-right"]}`}
          />
        </div>
      </Link>
    ) : (
      <div className={styles["nav-placeholder"]} />
    )}
  </div>
);
