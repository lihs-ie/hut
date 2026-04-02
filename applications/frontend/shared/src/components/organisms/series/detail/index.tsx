import { Series, SeriesSlug } from "@shared/domains/series";
import { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { Tag } from "@shared/domains/attributes/tag";
import { SeriesDetailPresenter } from "./index.presenter";

export type Props = {
  slug: SeriesSlug;
  findBySlug: (slug: string) => Promise<Series>;
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const SeriesDetail = async (props: Props) => {
  const series = await props.findBySlug(props.slug);
  const [chapters, tags] = await Promise.all([
    props.findChaptersByIdentifiers(series.chapters),
    props.findAllTags(series.tags),
  ]);

  return (
    <SeriesDetailPresenter
      series={series}
      slug={props.slug}
      chapters={chapters}
      tags={tags}
    />
  );
};
