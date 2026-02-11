import { LineChartPanel } from "@shared/components/molecules/chart/line";
import { BarChartPanel } from "@shared/components/molecules/chart/bar";
import type { TrendPoint, Distribution } from "@shared/domains/analytics/common";
import styles from "./grid.module.css";

const CONTENT_TYPE_DISPLAY_NAMES: Record<string, string> = {
  article: "記事",
  memo: "メモ",
  series: "シリーズ",
};

type Props = {
  getPageViewTrend: (period: string) => Promise<TrendPoint[]>;
  getContentTypeComparison: (period: string) => Promise<Distribution[]>;
  period: string;
};

export const PageViewTrend = async (props: Props) => {
  const [pageViewTrendRaw, contentTypeComparisonRaw] = await Promise.all([
    props.getPageViewTrend(props.period),
    props.getContentTypeComparison(props.period),
  ]);

  const pageViewTrend = pageViewTrendRaw.map((point) => ({
    label: point.dateKey,
    value: point.value,
  }));

  const contentTypeComparison = contentTypeComparisonRaw.map(
    (distribution) => ({
      label: CONTENT_TYPE_DISPLAY_NAMES[distribution.label] ?? distribution.label,
      value: distribution.value,
    })
  );

  return (
    <div className={styles.container}>
      <LineChartPanel title="PV推移" data={pageViewTrend} />
      <BarChartPanel
        title="コンテンツタイプ別PV"
        data={contentTypeComparison}
        axisLabel={{ x: "コンテンツタイプ", y: "PV数" }}
      />
    </div>
  );
};
