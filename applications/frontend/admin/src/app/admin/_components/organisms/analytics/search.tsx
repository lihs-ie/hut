import { LineChartPanel } from "@shared/components/molecules/chart/line";
import { RankingTable } from "@shared/components/molecules/list/ranking";
import type { TrendPoint, RankedItem } from "@shared/domains/analytics/common";
import styles from "./search.module.css";

type Props = {
  getSearchCountTrend: (period: string) => Promise<TrendPoint[]>;
  getSearchKeywordRanking: (period: string) => Promise<RankedItem[]>;
  getZeroHitKeywords: (period: string) => Promise<RankedItem[]>;
  period: string;
};

export const SearchAnalyticsSection = async (props: Props) => {
  const [searchCountTrendRaw, searchKeywordRanking, zeroHitKeywords] =
    await Promise.all([
      props.getSearchCountTrend(props.period),
      props.getSearchKeywordRanking(props.period),
      props.getZeroHitKeywords(props.period),
    ]);

  const searchCountTrend = searchCountTrendRaw.map((point) => ({
    label: point.dateKey,
    value: point.value,
  }));

  return (
    <div className={styles.container}>
      <LineChartPanel title="検索回数推移" data={searchCountTrend} />
      <div className={styles.rankings}>
        <RankingTable
          title="検索キーワード"
          items={searchKeywordRanking}
          valueFormatter={(value) => `${value}回`}
        />
        <RankingTable
          title="ゼロヒットキーワード"
          items={zeroHitKeywords}
          valueFormatter={(value) => `${value}回`}
        />
      </div>
    </div>
  );
};
