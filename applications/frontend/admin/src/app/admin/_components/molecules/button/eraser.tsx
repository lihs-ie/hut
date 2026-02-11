import styles from "./eraser.module.css";
import { EraserIcon } from "../../atoms/icon/eraser";
import { ButtonHTMLAttributes } from "react";

export type Props = {
  onClick: () => void;
  type?: ButtonHTMLAttributes<HTMLButtonElement>["type"];
  disabled?: boolean;
};

export const EraserButton = (props: Props) => (
  <button
    className={styles.container}
    onClick={props.onClick}
    type={props.type}
  >
    <div className={styles.icon}>
      <EraserIcon />
    </div>
  </button>
);
