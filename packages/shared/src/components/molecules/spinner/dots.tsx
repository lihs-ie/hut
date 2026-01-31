import { CommonImage } from "@shared/components/atoms/image/common";
import styles from "./dots.module.css";

export const DotsSpinner = () => (
  <div className={styles.container}>
    <CommonImage src="/spinner.svg" alt="Loading..." />
  </div>
);
