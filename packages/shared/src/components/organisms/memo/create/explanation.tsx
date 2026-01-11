import { CommonImage } from "@shared/components/atoms/image/common";
import styles from "./explanation.module.css";

export const CreateMemoExplanation = () => (
  <div className={styles.container}>
    <div className={styles.text}>
      <h1 className={styles.title}>New memo</h1>
    </div>
    <div className={styles.logo}>
      <CommonImage src="/logo/memo.svg" alt="Memo Logo" />
    </div>
  </div>
);
