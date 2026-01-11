import { MarkdownRenderer } from "@shared/components/global/mdx";
import { MemoEntryTextCard } from "@shared/components/molecules/list/card/memo/entry-text";
import { Memo as Aggregate } from "@shared/domains/memo";
import styles from "./index.module.css";
import { TagBadgeList } from "@shared/components/organisms/common/list/tag";
import { Tag } from "@shared/domains/attributes/tag";

export type Props = {
  memo: Aggregate;
  renderer: MarkdownRenderer;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const revalidate = 3600;

export const MemoPresenter = async (props: Props) => (
  <div className={styles.container}>
    <TagBadgeList
      findAllTags={props.findAllTags}
      identifiers={props.memo.tags}
    />
    {props.memo.entries.map((entry, index) => (
      <MemoEntryTextCard key={index} entry={entry} renderer={props.renderer} />
    ))}
  </div>
);
