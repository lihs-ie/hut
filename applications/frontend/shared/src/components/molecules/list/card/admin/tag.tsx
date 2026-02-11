import Link from "next/link";
import styles from "./tag.module.css";
import { formatDate } from "@shared/aspects/date";
import { CommonImage } from "@shared/components/atoms/image/common";

export type Props = {
  logo: string;
  name: string;
  createdAt: Date;
  href: string;
};

export const TagCard = (props: Props) => (
  <Link href={props.href} className={styles.container}>
    <div className={styles.logo}>
      <CommonImage src={props.logo} alt={props.name} />
    </div>
    <div className={styles.info}>
      <h3 className={styles.name}>{props.name}</h3>
      <div className={styles.meta}>
        <span className={styles["created-at"]}>
          {formatDate(props.createdAt)}
        </span>
      </div>
    </div>
  </Link>
);
