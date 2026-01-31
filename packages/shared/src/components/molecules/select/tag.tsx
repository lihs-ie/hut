import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";
import styles from "./tag.module.css";
import { TagBadge } from "../badge/tag";

export type Props = {
  tags: Tag[];
  onSelect: (tag: Tag) => void;
  selected: TagIdentifier[];
};

export const TagSelect = (props: Props) => (
  <div className={styles.container}>
    {props.tags.map((tag) => (
      <button
        key={tag.identifier}
        type="button"
        className={`${styles.button} ${
          props.selected.includes(tag.identifier) && styles["tag-selected"]
        }`}
        onClick={() => props.onSelect(tag)}
      >
        <TagBadge name={tag.name} logo={tag.logo} />
      </button>
    ))}
  </div>
);
