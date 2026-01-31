import styles from "./play.module.css";

export type Props = {
  className?: string;
};

export const PlayIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
