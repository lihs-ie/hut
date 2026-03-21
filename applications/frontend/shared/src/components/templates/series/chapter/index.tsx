import { Suspense } from "react";
import Link from "next/link";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { ChevronLeftIcon } from "@shared/components/atoms/icon/chevron-left";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";
import { ClockIcon } from "@shared/components/atoms/icon/clock";
import { BallpenIcon } from "@shared/components/atoms/icon/ballpen";
import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";
import { ModestText } from "@shared/components/atoms/text/modest";
import { Series, SeriesSlug, ChapterSlug } from "@shared/domains/series";
import { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { Routes } from "@shared/config/presentation/route";
import { formatDate } from "@shared/aspects/date";
import styles from "./index.module.css";

export type Props = {
  slug: SeriesSlug;
  chapterSlug: ChapterSlug;
  series: Series;
  renderer: MarkdownRenderer;
  findChapterBySlug: (slug: string) => Promise<Chapter>;
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
  editable?: boolean;
};

export const ChapterIndex = async (props: Props) => {
  const [currentChapter, allChapters] = await Promise.all([
    props.findChapterBySlug(props.chapterSlug),
    props.findChaptersByIdentifiers(props.series.chapters),
  ]);

  const currentIndex = allChapters.findIndex(
    (chapter) => chapter.slug === props.chapterSlug
  );

  if (currentIndex === -1) {
    return (
      <div className={styles.container}>
        <div className={styles.wrapper}>
          <p>チャプターが見つかりませんでした</p>
        </div>
      </div>
    );
  }

  const prevChapter = currentIndex > 0 ? allChapters[currentIndex - 1] : null;
  const nextChapter =
    currentIndex < allChapters.length - 1
      ? allChapters[currentIndex + 1]
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
                {props.series.title}
              </Link>

              <nav className={styles["nav-list"]} aria-label="チャプター一覧">
                {allChapters.map((chapter, index) => {
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
                <div className={styles["chapter-label-row"]}>
                  <p className={styles["chapter-label"]}>
                    Chapter {String(currentIndex + 1).padStart(2, "0")}
                  </p>
                  {props.editable && (
                    <Link
                      href={Routes.page.series.chapter.edit(props.slug, props.chapterSlug)}
                      className={styles["chapter-edit-button"]}
                    >
                      <BallpenIcon className={styles["chapter-edit-button-icon"]} />
                      編集
                    </Link>
                  )}
                </div>
                <h1 className={styles["chapter-title"]}>{currentChapter.title}</h1>
                <div className={styles["chapter-meta"]}>
                  <ModestText>
                    <span className={styles["meta-icon"]}>
                      <ClockIcon />
                    </span>
                    投稿日時：{formatDate(currentChapter.timeline.createdAt)}
                  </ModestText>
                  <ModestText>
                    <span className={styles["meta-icon"]}>
                      <ClockIcon />
                    </span>
                    最終更新日時：{formatDate(currentChapter.timeline.updatedAt)}
                  </ModestText>
                </div>
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
                      <span className={styles["nav-button-label"]}>前の章</span>
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
                      <span className={styles["nav-button-label"]}>次の章</span>
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
