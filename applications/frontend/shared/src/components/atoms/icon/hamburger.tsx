import styles from "./hamburger.module.css";

export type Props = {
  className?: string;
};

export const HamburgerIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);

export const MenuIcon = HamburgerIcon;
