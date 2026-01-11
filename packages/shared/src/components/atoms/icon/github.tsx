import styles from "./github.module.css";

export type Props = {
  className?: string;
};

export const GithubIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
