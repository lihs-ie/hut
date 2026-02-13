import { Series } from "@shared/domains/series";
import { SeriesSummaryCard } from "@shared/components/molecules/list/card/series/summary";
import styles from "./index.module.css";

export type Props = {
  seriesList: Series[];
};

export const SeriesList = (props: Props) => {
  if (props.seriesList.length === 0) {
    return (
      <div className={styles.container}>
        <div className={styles.empty}>
          <p className={styles["empty-title"]}>シリーズがありません</p>
          <p className={styles["empty-description"]}>
            シリーズが公開されるとここに表示されます
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className={styles.container}>
      <div className={styles.grid}>
        {props.seriesList.map((series) => (
          <SeriesSummaryCard
            key={series.identifier}
            slug={series.slug}
            title={series.title}
            description={series.description}
            cover={series.cover}
            tags={series.tags}
            chapterCount={series.chapters.length}
          />
        ))}
      </div>
    </div>
  );
};
