import { Series } from "@shared/domains/series";
import { Tag } from "@shared/domains/attributes/tag";
import { SeriesList } from "@shared/components/organisms/series/list";
import styles from "./index.module.css";

export type Props = {
  seriesList: Series[];
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  author?: {
    name: string;
    avatar?: string;
  };
};

export const SeriesListIndex = (props: Props) => {
  return (
    <div className={styles.container}>
      <div className={styles.wrapper}>
        <div className={styles.header}>
          <h1 className={styles.title}>シリーズ</h1>
          <p className={styles.description}>
            技術書や体系的にまとめられたコンテンツ
          </p>
        </div>

        <SeriesList seriesList={props.seriesList} findAllTags={props.findAllTags} author={props.author} />
      </div>
    </div>
  );
};
