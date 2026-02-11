import z from "zod";
import { AnalyticsDashboard } from "../../_components/templates/admin/analytics";
import {
  getTotalPageViews,
  getUniqueVisitors,
  getAverageDwellTime,
  getSearchCount,
  getPageViewTrend,
  getContentTypeComparison,
  getContentRanking,
  getReferrerRanking,
  getDeviceDistribution,
  getDwellTimeRanking,
  getScrollDepthDistribution,
  getSearchCountTrend,
  getSearchKeywordRanking,
  getZeroHitKeywords,
  getTagPageViews,
} from "@/actions/analytics";

type Props = {
  searchParams: Promise<{
    period?: string;
  }>;
};

const querySchema = z.object({
  period: z
    .enum(["7d", "30d", "90d", "all"])
    .optional()
    .default("30d"),
});

export default async function AnalyticsPage(props: Props) {
  const parameters = querySchema.parse(await props.searchParams);

  return (
    <AnalyticsDashboard
      period={parameters.period}
      getTotalPageViews={getTotalPageViews}
      getUniqueVisitors={getUniqueVisitors}
      getAverageDwellTime={getAverageDwellTime}
      getSearchCount={getSearchCount}
      getPageViewTrend={getPageViewTrend}
      getContentTypeComparison={getContentTypeComparison}
      getContentRanking={getContentRanking}
      getReferrerRanking={getReferrerRanking}
      getDeviceDistribution={getDeviceDistribution}
      getDwellTimeRanking={getDwellTimeRanking}
      getScrollDepthDistribution={getScrollDepthDistribution}
      getSearchCountTrend={getSearchCountTrend}
      getSearchKeywordRanking={getSearchKeywordRanking}
      getZeroHitKeywords={getZeroHitKeywords}
      getTagPageViews={getTagPageViews}
    />
  );
}
