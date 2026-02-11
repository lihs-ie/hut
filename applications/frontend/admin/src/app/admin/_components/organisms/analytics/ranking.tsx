import { RankingTable } from "@shared/components/molecules/list/ranking";
import type { RankedItem } from "@shared/domains/analytics/common";

type Props = {
  getContentRanking: (period: string) => Promise<RankedItem[]>;
  period: string;
};

export const ContentRanking = async (props: Props) => {
  const contentRanking = await props.getContentRanking(props.period);

  return (
    <RankingTable
      title="コンテンツPVランキング"
      items={contentRanking}
      valueFormatter={(value) => `${value} PV`}
    />
  );
};
