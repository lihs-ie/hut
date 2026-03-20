import { DotsSpinner } from "@shared/components/molecules/spinner/dots";
import styles from "./loading.module.css";

export const LoadingOverlay = () => (
  <div className={styles.container} role="status" aria-label="読み込み中">
    <DotsSpinner />
  </div>
);
