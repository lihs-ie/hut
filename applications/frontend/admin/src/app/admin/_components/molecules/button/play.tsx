import styles from "./play.module.css";
import { PlayIcon } from "../../atoms/icon/play";
import Link from "next/link";

export type Props = {
  href: string;
};

export const PlayButton = (props: Props) => (
  <Link className={styles.container} href={props.href}>
    <div className={styles.icon}>
      <PlayIcon />
    </div>
  </Link>
);
