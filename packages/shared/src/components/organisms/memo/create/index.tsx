import styles from "./index.module.css";
import { CreateMemoExplanation } from "./explanation";
import { MemoCreateForm } from "./form";
import { UnvalidatedMemo } from "@shared/domains/memo";

export type Props = {
  persist: (unvalidated: UnvalidatedMemo) => Promise<void>;
};

export const MemoCreate = (props: Props) => (
  <div className={styles.container}>
    <CreateMemoExplanation />
    <MemoCreateForm persist={props.persist} />
  </div>
);
