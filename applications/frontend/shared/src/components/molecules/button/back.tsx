import { ArrowLeftIcon } from "@shared/components/atoms/icon/arrow-left";
import styles from "./back.module.css";

export type Props = {
  onClick: () => void;
  disabled?: boolean;
};

export const BackButton = (props: Props) => (
  <button
    type="button"
    className={styles.container}
    onClick={props.onClick}
    disabled={props.disabled}
  >
    <ArrowLeftIcon className={styles.icon} />
  </button>
);
