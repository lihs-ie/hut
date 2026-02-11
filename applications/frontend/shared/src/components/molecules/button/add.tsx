import { PlusIcon } from "@shared/components/atoms/icon/plus";
import styles from "./add.module.css";

export type Props = {
  hasLabel: boolean;
  onClick: () => void;
};

export const AddButton = (props: Props) => (
  <button type="button" onClick={props.onClick} className={styles.container}>
    <PlusIcon className={styles.icon} />
    追加
  </button>
);
