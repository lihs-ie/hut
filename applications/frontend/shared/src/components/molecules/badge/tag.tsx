import { TagName } from "@shared/domains/attributes/tag";
import styles from "./tag.module.css";
import { CommonImage } from "@shared/components/atoms/image/common";
import { Image } from "@shared/domains/common/image";

export type Props = {
  name: TagName;
  logo: Image;
};

export const TagBadge = (props: Props) => (
  <span className={styles.container}>
    <div className={styles.image}>
      <CommonImage src={props.logo} alt={props.name} />
    </div>
    <span>{props.name}</span>
  </span>
);
