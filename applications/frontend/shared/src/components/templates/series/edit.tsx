import { SeriesEditOrganism } from "@shared/components/organisms/series/edit";
import { Series, SeriesSlug, UnvalidatedSeries } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { Tag } from "@shared/domains/attributes/tag";
import styles from "./edit.module.css";

export type Props = {
  initial?: Series;
  persist: (unvalidated: UnvalidatedSeries) => Promise<void>;
  tags: Tag[];
  chapters?: Chapter[];
  seriesSlug?: SeriesSlug;
};

export const SeriesEdit = (props: Props) => (
  <div className={styles.container}>
    <SeriesEditOrganism
      initial={props.initial}
      persist={props.persist}
      tags={props.tags}
      chapters={props.chapters}
      seriesSlug={props.seriesSlug}
    />
  </div>
);
