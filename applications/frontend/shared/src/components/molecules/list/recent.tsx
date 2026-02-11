import styles from "./recent.module.css";

export type RecentItem = {
  identifier: string;
  title: string;
  date: string;
  status: "published" | "draft" | "scheduled";
};

export type Props = {
  items: RecentItem[];
};

const statusLabels: Record<RecentItem["status"], string> = {
  published: "公開中",
  draft: "下書き",
  scheduled: "予約済",
};

export const RecentList = (props: Props) => (
  <div className={styles.container}>
    {props.items.map((item) => (
      <div key={item.identifier} className={styles.item}>
        <div className={styles.content}>
          <span className={styles.title}>{item.title}</span>
          <div className={styles.meta}>
            <span>{item.date}</span>
          </div>
        </div>
        <span
          className={`${styles.status} ${styles[`status-${item.status}`]}`}
        >
          {statusLabels[item.status]}
        </span>
      </div>
    ))}
  </div>
);
