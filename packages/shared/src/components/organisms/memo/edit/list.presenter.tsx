import { MemoEntry } from "@shared/domains/memo";
import styles from "./list.module.css";
import { CommonImage } from "@shared/components/atoms/image/common";
import { MemoEntryTextCard } from "@shared/components/molecules/list/card/memo/entry-text";
import { MarkdownRenderer } from "@shared/components/global/mdx";
import { SimpleCard } from "@shared/components/atoms/card/simple";

export type Props = {
  entries: MemoEntry[];
  renderer: MarkdownRenderer;
};

export const MemoEntriesPresenter = (props: Props) => (
  <div className={styles.container}>
    {props.entries.length === 0 ? (
      <SimpleCard className={styles.empty}>
        <div className={styles.image}>
          <CommonImage src="/logo/comment.svg" alt="No entries" />
        </div>
        <p className={styles.text}>コメントを追加しましょう</p>
      </SimpleCard>
    ) : (
      props.entries.map((entry, index) => (
        <MemoEntryTextCard
          key={index}
          entry={entry}
          renderer={props.renderer}
        />
      ))
    )}
  </div>
);
