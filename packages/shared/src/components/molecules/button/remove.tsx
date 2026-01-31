import { XIcon } from "@shared/components/atoms/icon";
import styles from "./remove.module.css";

export type Props = {
  onClick: () => void;
  ariaLabel?: string;
};

export const RemoveButton = (props: Props) => (
  <button
    type="button"
    onClick={props.onClick}
    className={styles.container}
    aria-label={props.ariaLabel}
  >
    <XIcon className={styles.icon} />
  </button>
);
