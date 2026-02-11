import styles from "./explanation.module.css";
import { TagBadgeList } from "../../common/list/tag";
import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";

export type Props = {
  title: string;
  tags: TagIdentifier[];
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  description: string;
};

export const SeriesExplanation = (props: Props) => (
  <div className={styles.container}>
    <h2 className={styles.title}>{props.title}</h2>
    <p className={styles.description}>{props.description}</p>
    <TagBadgeList identifiers={props.tags} findAllTags={props.findAllTags} />
  </div>
);
