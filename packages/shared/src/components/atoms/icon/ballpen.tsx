import styles from "./ballpen.module.css";

export type Props = {
  className?: string;
};

export const BallpenIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
