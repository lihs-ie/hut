import { SeriesSlug, ChapterSlug } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { ChevronLeftIcon } from "@shared/components/atoms/icon/chevron-left";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";
import { ClockIcon } from "@shared/components/atoms/icon/clock";
import { ModestText } from "@shared/components/atoms/text/modest";
import { NavigableLink } from "@shared/components/molecules/link/navigable";
import { Routes } from "@shared/config/presentation/route";
import { formatDate } from "@shared/aspects/date";
import { ReactNode } from "react";
import styles from "./content.module.css";

export type Props = {
  slug: SeriesSlug;
  chapterSlug: ChapterSlug;
  currentChapter: Chapter;
  currentIndex: number;
  prevChapter: Chapter | null;
  nextChapter: Chapter | null;
  renderedContent: ReactNode;
};

export const ChapterContentPresenter = (props: Props) => (
  <div className={styles.container}>
    <article>
      <div className={styles.header}>
        <p className={styles.label}>
          Chapter {String(props.currentIndex + 1).padStart(2, "0")}
        </p>
        <h1 className={styles.heading}>{props.currentChapter.title}</h1>
        <div className={styles.meta}>
          {props.currentChapter.publishedAt !== null && (
            <ModestText>
              <span className={styles.icon}>
                <ClockIcon />
              </span>
              投稿日時：{formatDate(props.currentChapter.publishedAt)}
            </ModestText>
          )}
          <ModestText>
            <span className={styles.icon}>
              <ClockIcon />
            </span>
            最終更新日時：{formatDate(props.currentChapter.timeline.updatedAt)}
          </ModestText>
        </div>
      </div>

      <div className={styles.body}>
        <div className="prose">{props.renderedContent}</div>
      </div>

      <nav className={styles.navigation} aria-label="前後のチャプター">
        {props.prevChapter && (
          <NavigableLink
            href={Routes.page.series.chapter.show(
              props.slug,
              props.prevChapter.slug,
            )}
            className={`${styles.button} ${styles.prev}`}
          >
            <ChevronLeftIcon className={styles.navicon} />
            <div className={styles.content}>
              <span className={styles.navlabel}>前の章</span>
              <span className={styles.navtitle}>
                {props.prevChapter.title}
              </span>
            </div>
          </NavigableLink>
        )}

        {props.nextChapter && (
          <NavigableLink
            href={Routes.page.series.chapter.show(
              props.slug,
              props.nextChapter.slug,
            )}
            className={`${styles.button} ${styles.next}`}
          >
            <div className={styles.content}>
              <span className={styles.navlabel}>次の章</span>
              <span className={styles.navtitle}>
                {props.nextChapter.title}
              </span>
            </div>
            <ChevronRightIcon className={styles.navicon} />
          </NavigableLink>
        )}
      </nav>
    </article>
  </div>
);
