import { MarkdownRenderer } from "@shared/components/global/mdx";
import { SeriesSlug, ChapterSlug, ChapterIdentifier } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { ChapterContentPresenter } from "./content.presenter";
import styles from "./content.module.css";

export type Props = {
  slug: SeriesSlug;
  chapterSlug: ChapterSlug;
  seriesChapterIdentifiers: ChapterIdentifier[];
  renderer: MarkdownRenderer;
  findChapterBySlug: (slug: string) => Promise<Chapter>;
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
};

export const ChapterContent = async (props: Props) => {
  const currentChapter = await props.findChapterBySlug(props.chapterSlug);

  const [allChapters, renderedContent] = await Promise.all([
    props.findChaptersByIdentifiers(props.seriesChapterIdentifiers),
    props.renderer(currentChapter.content),
  ]);

  const currentIndex = allChapters.findIndex(
    (chapter) => chapter.slug === props.chapterSlug,
  );

  if (currentIndex === -1) {
    return (
      <div className={styles.container}>
        <p className={styles.notFound}>チャプターが見つかりませんでした</p>
      </div>
    );
  }

  const prevChapter = currentIndex > 0 ? allChapters[currentIndex - 1] : null;
  const nextChapter =
    currentIndex < allChapters.length - 1
      ? allChapters[currentIndex + 1]
      : null;

  return (
    <ChapterContentPresenter
      slug={props.slug}
      chapterSlug={props.chapterSlug}
      currentChapter={currentChapter}
      currentIndex={currentIndex}
      prevChapter={prevChapter}
      nextChapter={nextChapter}
      renderedContent={renderedContent}
    />
  );
};
