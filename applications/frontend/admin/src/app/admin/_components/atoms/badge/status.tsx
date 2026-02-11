import { PublishStatus } from "@shared/domains/common";
import styles from "./status.module.css";

export type Props = {
  status: PublishStatus;
};

const labels: Record<string, string> = {
  [PublishStatus.DRAFT]: "下書き",
  [PublishStatus.PUBLISHED]: "公開中",
  [PublishStatus.ARCHIVED]: "非公開",
};

export const PublishStatusBadge = (props: Props) => (
  <span className={`${styles.container} ${styles[props.status]}`}>
    {labels[props.status]}
  </span>
);
