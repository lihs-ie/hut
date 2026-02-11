import { RankingTable } from "@shared/components/molecules/list/ranking";
import { BarChartPanel } from "@shared/components/molecules/chart/bar";
import { formatDwellTime, dwellTimeSchema } from "@shared/domains/analytics/engagement";
import type { RankedItem, Distribution } from "@shared/domains/analytics/common";
import styles from "./grid.module.css";

type Props = {
  getDwellTimeRanking: (period: string) => Promise<RankedItem[]>;
  getScrollDepthDistribution: (period: string) => Promise<Distribution[]>;
  period: string;
};

export const EngagementSection = async (props: Props) => {
  const [dwellTimeRanking, scrollDepthDistribution] = await Promise.all([
    props.getDwellTimeRanking(props.period),
    props.getScrollDepthDistribution(props.period),
  ]);

  return (
    <div className={styles.container}>
      <RankingTable
        title="平均滞在時間ランキング"
        items={dwellTimeRanking}
        valueFormatter={(value) => formatDwellTime(dwellTimeSchema.parse(value))}
      />
      <BarChartPanel
        title="スクロール深度分布"
        data={scrollDepthDistribution}
        axisLabel={{ x: "深度", y: "セッション数" }}
      />
    </div>
  );
};
