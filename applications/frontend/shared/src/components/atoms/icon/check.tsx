import styles from "./check.module.css";

export type Props = {
  className?: string;
};

export const CheckIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
