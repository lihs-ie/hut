import Link from "next/link";
import { ChapterSlug, SeriesSlug } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { ChevronLeftIcon } from "@shared/components/atoms/icon/chevron-left";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";
import { ClockIcon } from "@shared/components/atoms/icon/clock";
import { ModestText } from "@shared/components/atoms/text/modest";
import { Routes } from "@shared/config/presentation/route";
import { formatDate } from "@shared/aspects/date";
import { ReactNode } from "react";
import styles from "./index.presenter.module.css";

export type Props = {
  slug: SeriesSlug;
  chapterSlug: ChapterSlug;
  seriesTitle: string;
  currentChapter: Chapter;
  allChapters: Chapter[];
  currentIndex: number;
  prevChapter: Chapter | null;
  nextChapter: Chapter | null;
  renderedContent: ReactNode;
};

export const ChapterPresenter = (props: Props) => (
  <div className={styles.layout}>
    <aside className={styles.sidebar}>
      <div className={styles["sidebar-inner"]}>
        <Link href={Routes.page.series.show(props.slug)} className={styles["book-link"]}>
          <BookOpenIcon className={styles["book-link-icon"]} />
          {props.seriesTitle}
        </Link>

        <nav className={styles["nav-list"]} aria-label="チャプター一覧">
          {props.allChapters.map((chapter, index) => {
            const isActive = chapter.slug === props.chapterSlug;
            return (
              <Link
                key={chapter.slug}
                href={Routes.page.series.chapter.show(props.slug, chapter.slug)}
                className={`${styles["nav-item"]} ${isActive ? styles["nav-item-active"] : ""}`}
              >
                <span className={styles["nav-item-number"]}>{String(index + 1).padStart(2, "0")}</span>
                <span className={styles["nav-item-title"]}>{chapter.title}</span>
              </Link>
            );
          })}
        </nav>
      </div>
    </aside>

    <div className={styles.main}>
      <article>
        <div className={styles["chapter-header"]}>
          <p className={styles["chapter-label"]}>
            Chapter {String(props.currentIndex + 1).padStart(2, "0")}
          </p>
          <h1 className={styles["chapter-title"]}>{props.currentChapter.title}</h1>
          <div className={styles["chapter-meta"]}>
            <ModestText>
              <span className={styles["meta-icon"]}>
                <ClockIcon />
              </span>
              投稿日時：{formatDate(props.currentChapter.timeline.createdAt)}
            </ModestText>
            <ModestText>
              <span className={styles["meta-icon"]}>
                <ClockIcon />
              </span>
              最終更新日時：{formatDate(props.currentChapter.timeline.updatedAt)}
            </ModestText>
          </div>
        </div>

        <div className={styles["content-body"]}>
          <div className="prose">{props.renderedContent}</div>
        </div>

        <nav className={styles.navigation} aria-label="前後のチャプター">
          {props.prevChapter && (
            <Link
              href={Routes.page.series.chapter.show(props.slug, props.prevChapter.slug)}
              className={`${styles["nav-button"]} ${styles["nav-button-prev"]}`}
            >
              <ChevronLeftIcon className={styles["nav-button-icon"]} />
              <div className={styles["nav-button-content"]}>
                <span className={styles["nav-button-label"]}>前の章</span>
                <span className={styles["nav-button-title"]}>
                  {props.prevChapter.title}
                </span>
              </div>
            </Link>
          )}

          {props.nextChapter && (
            <Link
              href={Routes.page.series.chapter.show(props.slug, props.nextChapter.slug)}
              className={`${styles["nav-button"]} ${styles["nav-button-next"]}`}
            >
              <div className={styles["nav-button-content"]}>
                <span className={styles["nav-button-label"]}>次の章</span>
                <span className={styles["nav-button-title"]}>
                  {props.nextChapter.title}
                </span>
              </div>
              <ChevronRightIcon className={styles["nav-button-icon"]} />
            </Link>
          )}
        </nav>
      </article>
    </div>
  </div>
);
