import {
  ContentSection,
  type Props as ContentSectionProps,
} from "@shared/components/organisms/common/top/search";
import { Series } from "@shared/domains/series";
import { Tag } from "@shared/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token";
import styles from "./index.module.css";

export type Props = {
  search: () => Promise<Series[]>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  linkMode?: ContentSectionProps<Series>["linkMode"];
};

export const SeriesListIndex = (props: Props) => (
  <div className={styles.container}>
    <ContentSection
      search={props.search}
      findAllTags={props.findAllTags}
      slugOf={(series) => series.slug}
      titleOf={(series) => series.title}
      dateOf={(series) => series.publishedAt}
      type={ContentType.SERIES}
      maxItems={Infinity}
      linkMode={props.linkMode}
    />
  </div>
);
