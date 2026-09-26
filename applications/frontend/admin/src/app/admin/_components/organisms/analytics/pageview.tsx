import { LineChartPanel } from "@shared/components/molecules/chart/line";
import type { TrendPoint } from "@shared/domains/analytics/common";

type Props = {
  getPageViewTrend: (period: string) => Promise<TrendPoint[]>;
  period: string;
};

export const PageViewTrend = async (props: Props) => {
  const pageViewTrendRaw = await props.getPageViewTrend(props.period);

  const pageViewTrend = pageViewTrendRaw.map((point) => ({
    label: point.dateKey,
    value: point.value,
  }));

  return <LineChartPanel title="PV推移" data={pageViewTrend} />;
};
