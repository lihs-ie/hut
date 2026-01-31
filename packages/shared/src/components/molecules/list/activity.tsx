import styles from "./activity.module.css";

export type ActivityItem = {
  identifier: string;
  type: "create" | "update" | "delete" | "publish";
  message: string;
  timestamp: string;
};

export type Props = {
  activities: ActivityItem[];
};

export const ActivityList = (props: Props) => (
  <div className={styles.container}>
    {props.activities.map((activity, index) => (
      <div
        key={activity.identifier}
        className={`${styles.item} ${styles[`type-${activity.type}`]}`}
      >
        <div className={styles.indicator}>
          <span className={styles.dot} />
          {index < props.activities.length - 1 && (
            <span className={styles.line} />
          )}
        </div>
        <div className={styles.content}>
          <span className={styles.message}>{activity.message}</span>
          <span className={styles.timestamp}>{activity.timestamp}</span>
        </div>
      </div>
    ))}
  </div>
);
