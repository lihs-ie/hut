import styles from "./layout-dashboard.module.css";

export type Props = {
  className?: string;
};

export const LayoutDashboardIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
