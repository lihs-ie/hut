import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ChapterSlug, Series, SeriesSlug } from "@shared/domains/series";
import type { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { ChapterPresenter } from "./index.presenter";

export type Props = {
  seriesSlug: SeriesSlug;
  chapterSlug: ChapterSlug;
  findBySlug: (slug: SeriesSlug) => Promise<Series>;
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
  renderer: MarkdownRenderer;
};

export const SeriesChapter = async (props: Props) => {
  const series = await props.findBySlug(props.seriesSlug);
  const chapters = await props.findChaptersByIdentifiers(series.chapters);

  return (
    <ChapterPresenter
      seriesTitle={series.title}
      seriesSlug={props.seriesSlug}
      chapterSlug={props.chapterSlug}
      chapters={chapters}
      renderer={props.renderer}
    />
  );
};

export { ChapterPresenter };
