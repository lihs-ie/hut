import styles from "./facing-book.module.css";

export type Props = {
  className?: string;
};

export const FacingBookIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);

export const BookOpenIcon = FacingBookIcon;
