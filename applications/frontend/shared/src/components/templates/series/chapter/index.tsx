import { Suspense } from "react";
import Link from "next/link";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { ChevronLeftIcon } from "@shared/components/atoms/icon/chevron-left";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";
import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";
import { Series, SeriesSlug, ChapterSlug } from "@shared/domains/series";
import { Routes } from "@shared/config/presentation/route";
import styles from "./index.module.css";

export type Props = {
  slug: SeriesSlug;
  chapterSlug: ChapterSlug;
  series: Series;
  renderer: MarkdownRenderer;
};

export const ChapterIndex = async (props: Props) => {
  const series = props.series;
  const currentIndex = series.chapters.findIndex(
    (ch) => ch.slug === props.chapterSlug
  );
  const currentChapter = series.chapters[currentIndex];

  if (!currentChapter) {
    return (
      <div className={styles.container}>
        <div className={styles.wrapper}>
          <p>チャプターが見つかりませんでした</p>
        </div>
      </div>
    );
  }

  const prevChapter = currentIndex > 0 ? series.chapters[currentIndex - 1] : null;
  const nextChapter =
    currentIndex < series.chapters.length - 1
      ? series.chapters[currentIndex + 1]
      : null;

  const RenderedContent = await props.renderer(currentChapter.content);

  return (
    <div className={styles.container}>
      <div className={styles.wrapper}>
        <div className={styles.layout}>
          <aside className={styles.sidebar}>
            <div className={styles["sidebar-inner"]}>
              <Link href={Routes.page.series.show(props.slug)} className={styles["book-link"]}>
                <BookOpenIcon className={styles["book-link-icon"]} />
                {series.title}
              </Link>

              <nav className={styles["nav-list"]} aria-label="チャプター一覧">
                {series.chapters.map((chapter, index) => {
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
                  Chapter {String(currentIndex + 1).padStart(2, "0")}
                </p>
                <h1 className={styles["chapter-title"]}>{currentChapter.title}</h1>
              </div>

              <div className={styles["content-body"]}>
                <Suspense fallback={<ArticleContentSkeleton />}>
                  <div className="prose">{RenderedContent}</div>
                </Suspense>
              </div>

              <nav className={styles.navigation} aria-label="前後のチャプター">
                {prevChapter && (
                  <Link
                    href={Routes.page.series.chapter.show(props.slug, prevChapter.slug)}
                    className={`${styles["nav-button"]} ${styles["nav-button-prev"]}`}
                  >
                    <ChevronLeftIcon className={styles["nav-button-icon"]} />
                    <div className={styles["nav-button-content"]}>
                      <span className={styles["nav-button-label"]}>PREV</span>
                      <span className={styles["nav-button-title"]}>
                        {prevChapter.title}
                      </span>
                    </div>
                  </Link>
                )}

                {nextChapter && (
                  <Link
                    href={Routes.page.series.chapter.show(props.slug, nextChapter.slug)}
                    className={`${styles["nav-button"]} ${styles["nav-button-next"]}`}
                  >
                    <div className={styles["nav-button-content"]}>
                      <span className={styles["nav-button-label"]}>NEXT</span>
                      <span className={styles["nav-button-title"]}>
                        {nextChapter.title}
                      </span>
                    </div>
                    <ChevronRightIcon className={styles["nav-button-icon"]} />
                  </Link>
                )}
              </nav>
            </article>
          </div>
        </div>
      </div>
    </div>
  );
};
