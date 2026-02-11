import { BarChartPanel } from "@shared/components/molecules/chart/bar";
import type { RankedItem } from "@shared/domains/analytics/common";

type Props = {
  getTagPageViews: (period: string) => Promise<RankedItem[]>;
  period: string;
};

export const TagPageViewSection = async (props: Props) => {
  const tagPageViews = await props.getTagPageViews(props.period);

  return (
    <BarChartPanel
      title="タグ別PV"
      data={tagPageViews}
      axisLabel={{ x: "タグ", y: "PV数" }}
    />
  );
};
