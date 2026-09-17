import { MarkdownRenderer } from "@shared/components/global/mdx";
import { SeriesSlug, ChapterSlug, ChapterIdentifier } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { ChapterContentPresenter } from "./content.presenter";
import styles from "./content.module.css";

export type Props = {
  slug: SeriesSlug;
  chapterSlug: ChapterSlug;
  chapters: ChapterIdentifier[];
  renderer: MarkdownRenderer;
  findChapterBySlug: (slug: string) => Promise<Chapter>;
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
};

export const ChapterContent = async (props: Props) => {
  const currentChapter = await props.findChapterBySlug(props.chapterSlug);

  const [chapters, renderedContent] = await Promise.all([
    props.findChaptersByIdentifiers(props.chapters),
    props.renderer(currentChapter.content),
  ]);

  const currentIndex = chapters.findIndex(
    (chapter) => chapter.slug === props.chapterSlug,
  );

  if (currentIndex === -1) {
    return (
      <div className={styles.container}>
        <p className={styles.notFound}>チャプターが見つかりませんでした</p>
      </div>
    );
  }

  const prevChapter = currentIndex > 0 ? chapters[currentIndex - 1] : null;
  const nextChapter =
    currentIndex < chapters.length - 1 ? chapters[currentIndex + 1] : null;

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
