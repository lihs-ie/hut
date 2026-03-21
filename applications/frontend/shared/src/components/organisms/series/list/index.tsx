import { Series } from "@shared/domains/series";
import { Tag } from "@shared/domains/attributes/tag";
import {
  SeriesSummaryCard,
  Props as SeriesSummaryCardProps,
} from "@shared/components/molecules/list/card/series/summary";
import styles from "./index.module.css";

export type Props = {
  seriesList: Series[];
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  author?: {
    name: string;
    avatar?: string;
  };
};

export const SeriesList = async (props: Props) => {
  if (props.seriesList.length === 0) {
    return (
      <div className={styles.container}>
        <div className={styles.empty}>
          <p className={styles["empty-title"]}>連載がありません</p>
          <p className={styles["empty-description"]}>
            連載が公開されるとここに表示されます
          </p>
        </div>
      </div>
    );
  }

  const allTagIdentifiers = [...new Set(props.seriesList.flatMap((series) => series.tags))];
  const allTags = await props.findAllTags(allTagIdentifiers);
  const tagNameMap = new Map(allTags.map((tag) => [tag.identifier, tag.name]));

  return (
    <div className={styles.container}>
      <div className={styles.grid}>
        {props.seriesList.map((series) => {
          const tagNames = series.tags
            .map((identifier) => tagNameMap.get(identifier))
            .filter((name) => name !== undefined);
          const cardProps: SeriesSummaryCardProps = {
            slug: series.slug,
            title: series.title,
            description: series.description,
            cover: series.cover,
            tags: tagNames,
            chapterCount: series.chapters.length,
            author: props.author,
          };
          return <SeriesSummaryCard key={series.identifier} {...cardProps} />;
        })}
      </div>
    </div>
  );
};
