import styles from "./edit.module.css";
import Link from "next/link";
import { PenIcon } from "../../atoms/icon/pen";

export type Props = {
  href: string;
};

export const EditButton = (props: Props) => (
  <Link className={styles.container} href={props.href}>
    <div className={styles.icon}>
      <PenIcon />
    </div>
  </Link>
);
