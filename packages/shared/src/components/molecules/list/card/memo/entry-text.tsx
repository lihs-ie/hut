import { MarkdownRenderer } from "@shared/components/global/mdx";
import { MemoEntry } from "@shared/domains/memo";
import styles from "./entry-text.module.css";
import { ModestText } from "@shared/components/atoms/text/modest";
import { ClockIcon } from "@shared/components/atoms/icon";
import { formatDateTime } from "@shared/aspects/date";

export type Props = {
  entry: MemoEntry;
  renderer: MarkdownRenderer;
};

export const MemoEntryTextCard = (props: Props) => (
  <section className={styles.container}>
    <ModestText>
      <i className={styles.icon}>
        <ClockIcon />
      </i>
      {formatDateTime(props.entry.createdAt)}
    </ModestText>
    {props.renderer(props.entry.text)}
  </section>
);
