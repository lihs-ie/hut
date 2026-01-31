import {
  ContentSection,
  type Props as ContentSectionProps,
} from "@shared/components/organisms/common/top/search";
import { Article } from "@shared/domains/articles";
import { Tag } from "@shared/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token";
import styles from "./list.module.css";

export type Props = {
  search: () => Promise<Article[]>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  linkMode?: ContentSectionProps<Article>["linkMode"];
};

export const ArticleListIndex = (props: Props) => (
  <div className={styles.container}>
    <ContentSection
      search={props.search}
      findAllTags={props.findAllTags}
      slugOf={(article) => article.slug}
      titleOf={(article) => article.title}
      dateOf={(article) => article.timeline.createdAt}
      type={ContentType.ARTICLE}
      maxItems={Infinity}
      linkMode={props.linkMode}
    />
  </div>
);
