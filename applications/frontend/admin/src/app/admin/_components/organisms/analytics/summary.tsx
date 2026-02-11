import { StatsCard } from "@shared/components/atoms/card/stats";
import {
  calculateTrendPercentage,
  type PeriodComparison,
} from "@shared/domains/analytics/common";
import { formatDwellTime, dwellTimeSchema } from "@shared/domains/analytics/engagement";
import styles from "./summary.module.css";

type Props = {
  getTotalPageViews: (period: string) => Promise<PeriodComparison>;
  getUniqueVisitors: (period: string) => Promise<PeriodComparison>;
  getAverageDwellTime: (period: string) => Promise<PeriodComparison>;
  getSearchCount: (period: string) => Promise<PeriodComparison>;
  period: string;
};

export const AnalyticsSummary = async (props: Props) => {
  const [totalPageViews, uniqueVisitors, averageDwellTime, searchCount] =
    await Promise.all([
      props.getTotalPageViews(props.period),
      props.getUniqueVisitors(props.period),
      props.getAverageDwellTime(props.period),
      props.getSearchCount(props.period),
    ]);

  return (
    <div className={styles.container}>
      <StatsCard
        title="総PV"
        value={totalPageViews.current}
        trend={{
          value: calculateTrendPercentage(
            totalPageViews.current,
            totalPageViews.previous
          ),
          label: "前期間比",
        }}
      />
      <StatsCard
        title="ユニークビジター"
        value={uniqueVisitors.current}
        trend={{
          value: calculateTrendPercentage(
            uniqueVisitors.current,
            uniqueVisitors.previous
          ),
          label: "前期間比",
        }}
      />
      <StatsCard
        title="平均滞在時間"
        value={formatDwellTime(dwellTimeSchema.parse(averageDwellTime.current))}
        trend={{
          value: calculateTrendPercentage(
            averageDwellTime.current,
            averageDwellTime.previous
          ),
          label: "前期間比",
        }}
      />
      <StatsCard
        title="検索数"
        value={searchCount.current}
        trend={{
          value: calculateTrendPercentage(searchCount.current, searchCount.previous),
          label: "前期間比",
        }}
      />
    </div>
  );
};
