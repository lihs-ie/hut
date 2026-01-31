import styles from "./eraser.module.css";

export type Props = {
  className?: string;
};

export const EraserIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
