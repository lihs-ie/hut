import { BarChartPanel } from "@shared/components/molecules/chart/bar";
import { PieChartPanel } from "@shared/components/molecules/chart/pie";
import type { RankedItem, Distribution } from "@shared/domains/analytics/common";
import styles from "./grid.module.css";

type Props = {
  getReferrerRanking: (period: string) => Promise<RankedItem[]>;
  getDeviceDistribution: (period: string) => Promise<Distribution[]>;
  period: string;
};

export const AccessSourceSection = async (props: Props) => {
  const [referrerRanking, deviceDistribution] = await Promise.all([
    props.getReferrerRanking(props.period),
    props.getDeviceDistribution(props.period),
  ]);

  return (
    <div className={styles.container}>
      <BarChartPanel
        title="リファラー分析"
        data={referrerRanking}
        layout="horizontal"
        axisLabel={{ x: "PV数", y: "リファラー" }}
      />
      <PieChartPanel title="デバイス比率" data={deviceDistribution} />
    </div>
  );
};
