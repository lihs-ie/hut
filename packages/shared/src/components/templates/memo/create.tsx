import { MemoCreate } from "@shared/components/organisms/memo/create";
import { UnvalidatedMemo } from "@shared/domains/memo";
import styles from "./create.module.css";

export type Props = {
  persist: (unvalidated: UnvalidatedMemo) => Promise<void>;
};

export const CreateMemoIndex = (props: Props) => (
  <div className={styles.container}>
    <MemoCreate persist={props.persist} />
  </div>
);
