import styles from "./rss.module.css";

export type Props = {
  className?: string;
};

export const RssIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
