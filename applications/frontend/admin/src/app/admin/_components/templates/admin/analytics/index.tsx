import { Suspense } from "react";
import { AnalyticsPeriodSelector } from "../../../molecules/analytics/period";
import { AnalyticsSummary } from "../../../organisms/analytics/summary";
import { AnalyticsSummarySkeleton } from "../../../organisms/analytics/summary.skeleton";
import { PageViewTrend } from "../../../organisms/analytics/pageview";
import { PageViewTrendSkeleton } from "../../../organisms/analytics/pageview.skeleton";
import { ContentRanking } from "../../../organisms/analytics/ranking";
import { ContentRankingSkeleton } from "../../../organisms/analytics/ranking.skeleton";
import { AccessSourceSection } from "../../../organisms/analytics/source";
import { AccessSourceSectionSkeleton } from "../../../organisms/analytics/source.skeleton";
import { EngagementSection } from "../../../organisms/analytics/engagement";
import { EngagementSectionSkeleton } from "../../../organisms/analytics/engagement.skeleton";
import { SearchAnalyticsSection } from "../../../organisms/analytics/search";
import { SearchAnalyticsSectionSkeleton } from "../../../organisms/analytics/search.skeleton";
import { TagPageViewSection } from "../../../organisms/analytics/tag";
import { TagPageViewSectionSkeleton } from "../../../organisms/analytics/tag.skeleton";
import type {
  PeriodComparison,
  TrendPoint,
  Distribution,
  RankedItem,
} from "@shared/domains/analytics/common";
import styles from "./index.module.css";

type PeriodComparisonFetcher = (period: string) => Promise<PeriodComparison>;
type TrendPointFetcher = (period: string) => Promise<TrendPoint[]>;
type DistributionFetcher = (period: string) => Promise<Distribution[]>;
type RankedItemFetcher = (period: string) => Promise<RankedItem[]>;

type Props = {
  period: string;
  getTotalPageViews: PeriodComparisonFetcher;
  getUniqueVisitors: PeriodComparisonFetcher;
  getAverageDwellTime: PeriodComparisonFetcher;
  getSearchCount: PeriodComparisonFetcher;
  getPageViewTrend: TrendPointFetcher;
  getContentTypeComparison: DistributionFetcher;
  getContentRanking: RankedItemFetcher;
  getReferrerRanking: RankedItemFetcher;
  getDeviceDistribution: DistributionFetcher;
  getDwellTimeRanking: RankedItemFetcher;
  getScrollDepthDistribution: DistributionFetcher;
  getSearchCountTrend: TrendPointFetcher;
  getSearchKeywordRanking: RankedItemFetcher;
  getZeroHitKeywords: RankedItemFetcher;
  getTagPageViews: RankedItemFetcher;
};

export const AnalyticsDashboard = async (props: Props) => {
  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <h1 className={styles.title}>統計ダッシュボード</h1>
        <AnalyticsPeriodSelector />
      </div>

      <Suspense fallback={<AnalyticsSummarySkeleton />}>
        <AnalyticsSummary
          period={props.period}
          getTotalPageViews={props.getTotalPageViews}
          getUniqueVisitors={props.getUniqueVisitors}
          getAverageDwellTime={props.getAverageDwellTime}
          getSearchCount={props.getSearchCount}
        />
      </Suspense>

      <Suspense fallback={<PageViewTrendSkeleton />}>
        <PageViewTrend
          period={props.period}
          getPageViewTrend={props.getPageViewTrend}
          getContentTypeComparison={props.getContentTypeComparison}
        />
      </Suspense>

      <Suspense fallback={<ContentRankingSkeleton />}>
        <ContentRanking
          period={props.period}
          getContentRanking={props.getContentRanking}
        />
      </Suspense>

      <Suspense fallback={<AccessSourceSectionSkeleton />}>
        <AccessSourceSection
          period={props.period}
          getReferrerRanking={props.getReferrerRanking}
          getDeviceDistribution={props.getDeviceDistribution}
        />
      </Suspense>

      <Suspense fallback={<EngagementSectionSkeleton />}>
        <EngagementSection
          period={props.period}
          getDwellTimeRanking={props.getDwellTimeRanking}
          getScrollDepthDistribution={props.getScrollDepthDistribution}
        />
      </Suspense>

      <Suspense fallback={<SearchAnalyticsSectionSkeleton />}>
        <SearchAnalyticsSection
          period={props.period}
          getSearchCountTrend={props.getSearchCountTrend}
          getSearchKeywordRanking={props.getSearchKeywordRanking}
          getZeroHitKeywords={props.getZeroHitKeywords}
        />
      </Suspense>

      <Suspense fallback={<TagPageViewSectionSkeleton />}>
        <TagPageViewSection
          period={props.period}
          getTagPageViews={props.getTagPageViews}
        />
      </Suspense>
    </div>
  );
};
