import styles from "./globe.module.css";

export type Props = {
  className?: string;
};

export const GlobeIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
