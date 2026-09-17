import { SeriesSlug } from "@shared/domains/series";
import { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { ChapterTOCPresenter } from "./presenter";

export type Props = {
  slug: SeriesSlug;
  seriesTitle: string;
  chapters: ChapterIdentifier[];
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
};

export const ChapterTOC = async (props: Props) => {
  const chapters = await props.findChaptersByIdentifiers(props.chapters);

  return (
    <ChapterTOCPresenter
      slug={props.slug}
      seriesTitle={props.seriesTitle}
      chapters={chapters}
    />
  );
};
