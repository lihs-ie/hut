import { MemoSlug, MemoTitle } from "@shared/domains/memo";
import Link from "next/link";
import styles from "./title.module.css";
import { ClockIcon } from "@shared/components/atoms/icon";
import { Timeline } from "@shared/domains/common";
import { ModestText } from "@shared/components/atoms/text/modest";
import { formatDateTime } from "@shared/aspects/date";

export type Props = {
  title: MemoTitle;
  timeline: Timeline;
  slug: MemoSlug;
};

export const MemoTitleCard = (props: Props) => (
  <Link className={styles.container} href={`/memos/${props.slug}`}>
    <h2 className={styles.title}>{props.title}</h2>
    <div className={styles.timeline}>
      <ModestText>
        <ClockIcon className={styles.icon} />
        &nbsp;{formatDateTime(props.timeline.createdAt)}
      </ModestText>
    </div>
  </Link>
);
