import { Career, Period } from "@shared/domains/user";
import styles from "./career.module.css";

export type Props = {
  career: Career;
};

export const formatPeriod = (period: Period): string => {
  const formatYearMonth = (date: Date): string => {
    const year = date.getFullYear();
    const month = date.getMonth() + 1;

    return `${year}年${month}月`;
  };

  const start = formatYearMonth(period.from);
  const end = period.to ? formatYearMonth(period.to) : "現在";

  return `${start} - ${end}`;
};
export const CareerCard = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.header}>
      <span className={styles.company}>{props.career.company}</span>
      <span className={styles.period}>{formatPeriod(props.career.period)}</span>
    </div>
    <div className={styles.body}>
      <span className={styles.role}>{props.career.role}</span>
      <p className={styles.description}>{props.career.description}</p>
    </div>
  </div>
);
