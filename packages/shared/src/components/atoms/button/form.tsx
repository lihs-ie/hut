import { ReactNode } from "react";
import styles from "./form.module.css";

export type Props = {
  onClick?: () => void;
  disabled?: boolean;
  children: ReactNode;
};

export const FormButton = (props: Props) => (
  <button
    className={styles.container}
    onClick={props.onClick}
    disabled={props.disabled}
    type="submit"
  >
    {props.children}
  </button>
);
