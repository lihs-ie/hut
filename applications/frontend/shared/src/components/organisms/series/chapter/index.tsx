import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ChapterSlug, Series, SeriesSlug } from "@shared/domains/series";
import type { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { ChapterPresenter } from "./index.presenter";
import styles from "./index.presenter.module.css";

export type Props = {
  slug: SeriesSlug;
  chapterSlug: ChapterSlug;
  series: Series;
  renderer: MarkdownRenderer;
  findChapterBySlug: (slug: string) => Promise<Chapter>;
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
};

export const ChapterContainer = async (props: Props) => {
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

  const renderedContent = await props.renderer(currentChapter.content);

  return (
    <ChapterPresenter
      slug={props.slug}
      chapterSlug={props.chapterSlug}
      seriesTitle={props.series.title}
      currentChapter={currentChapter}
      allChapters={allChapters}
      currentIndex={currentIndex}
      prevChapter={prevChapter}
      nextChapter={nextChapter}
      renderedContent={renderedContent}
    />
  );
};

export { ChapterPresenter };
