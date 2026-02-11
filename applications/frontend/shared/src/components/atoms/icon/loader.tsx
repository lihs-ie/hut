import styles from "./loader.module.css";

export type Props = {
  className?: string;
};

export const LoaderIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
