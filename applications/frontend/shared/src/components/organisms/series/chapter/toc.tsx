import { SeriesSlug } from "@shared/domains/series";
import { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { ChapterTocPresenter } from "./toc.presenter";

export type Props = {
  slug: SeriesSlug;
  seriesTitle: string;
  seriesChapterIdentifiers: ChapterIdentifier[];
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
};

export const ChapterToc = async (props: Props) => {
  const allChapters = await props.findChaptersByIdentifiers(
    props.seriesChapterIdentifiers,
  );

  return (
    <ChapterTocPresenter
      slug={props.slug}
      seriesTitle={props.seriesTitle}
      allChapters={allChapters}
    />
  );
};
