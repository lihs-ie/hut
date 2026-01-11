import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ChapterSlug, Series, SeriesSlug } from "@shared/domains/series";
import { ChapterPresenter } from "./index.presenter";

export type Props = {
  seriesSlug: SeriesSlug;
  chapterSlug: ChapterSlug;
  findBySlug: (slug: SeriesSlug) => Promise<Series>;
  renderer: MarkdownRenderer;
};

export const revalidate = 3600;

export const Chapter = async (props: Props) => {
  const series = await props.findBySlug(props.seriesSlug);

  return (
    <ChapterPresenter
      series={series}
      seriesSlug={props.seriesSlug}
      chapterSlug={props.chapterSlug}
      renderer={props.renderer}
    />
  );
};

export { ChapterPresenter };
