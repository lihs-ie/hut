import { Suspense } from "react";
import Link from "next/link";
import { BookOpenIcon, ChevronLeftIcon, ChevronRightIcon } from "@shared/components/atoms/icon";
import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";
import { Series, SeriesSlug, ChapterSlug } from "@shared/domains/series";
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
          {/* サイドバー - 目次 */}
          <aside className={styles.sidebar}>
            <div className={styles["sidebar-card"]}>
              <Link href={`/series/${props.slug}`} className={styles["book-link"]}>
                <BookOpenIcon className={styles["book-link-icon"]} />
                {series.title}
              </Link>

              <nav className={styles["nav-list"]}>
                {series.chapters.map((chapter, index) => {
                  const isActive = chapter.slug === props.chapterSlug;
                  return (
                    <Link
                      key={chapter.slug}
                      href={`/series/${props.slug}/chapters/${chapter.slug}`}
                      className={`${styles["nav-item"]} ${isActive ? styles["nav-item-active"] : ""}`}
                    >
                      <span className={styles["nav-item-number"]}>{index + 1}</span>
                      <span className={styles["nav-item-title"]}>{chapter.title}</span>
                    </Link>
                  );
                })}
              </nav>
            </div>
          </aside>

          {/* メインコンテンツ */}
          <main className={styles.main}>
            <article>
              <div className={styles["chapter-header"]}>
                <p className={styles["chapter-label"]}>
                  Chapter {currentIndex + 1}
                </p>
                <h1 className={styles["chapter-title"]}>{currentChapter.title}</h1>
              </div>

              {/* 本文 */}
              <div className={styles["content-card"]}>
                <Suspense fallback={<ArticleContentSkeleton />}>
                  <div className="prose">{RenderedContent}</div>
                </Suspense>
              </div>

              {/* ナビゲーション */}
              <div className={styles.navigation}>
                {prevChapter ? (
                  <Link
                    href={`/series/${props.slug}/chapters/${prevChapter.slug}`}
                    className={`${styles["nav-button"]} ${styles["nav-button-prev"]}`}
                  >
                    <ChevronLeftIcon className={styles["nav-button-icon"]} />
                    <div className={styles["nav-button-content"]}>
                      <span className={styles["nav-button-label"]}>前の章</span>
                      <span className={styles["nav-button-title"]}>
                        {prevChapter.title}
                      </span>
                    </div>
                  </Link>
                ) : (
                  <div className={styles["nav-placeholder"]} />
                )}

                {nextChapter ? (
                  <Link
                    href={`/series/${props.slug}/chapters/${nextChapter.slug}`}
                    className={`${styles["nav-button"]} ${styles["nav-button-next"]}`}
                  >
                    <div className={styles["nav-button-content"]}>
                      <span className={styles["nav-button-label"]}>次の章</span>
                      <span className={styles["nav-button-title"]}>
                        {nextChapter.title}
                      </span>
                    </div>
                    <ChevronRightIcon className={styles["nav-button-icon"]} />
                  </Link>
                ) : (
                  <div className={styles["nav-placeholder"]} />
                )}
              </div>
            </article>
          </main>
        </div>
      </div>
    </div>
  );
};
