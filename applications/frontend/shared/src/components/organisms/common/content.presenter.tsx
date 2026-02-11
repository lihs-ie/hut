import styles from "./content.module.css";
import { MarkdownRenderer } from "@shared/components/global/mdx";
import { TagBadgeList } from "@shared/components/organisms/common/list/tag";
import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";

export type Props<T> = {
  target: T;
  tagOf: (target: T) => TagIdentifier[];
  contentOf: (target: T) => string;
  renderer: MarkdownRenderer;
  findAllTags: (tags: string[]) => Promise<Tag[]>;
};

export const ContentPresenter = <T,>(props: Props<T>) => (
  <article className={styles.container}>
    <div className={styles.content}>
      <TagBadgeList
        identifiers={props.tagOf(props.target)}
        findAllTags={props.findAllTags}
      />
      {props.renderer(props.contentOf(props.target))}
      <div className={styles.toc}></div>
    </div>
  </article>
);
