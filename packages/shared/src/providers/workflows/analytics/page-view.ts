import { validatePageView } from "@shared/domains/analytics/page-view";
import { validatePeriod } from "@shared/domains/analytics/common";
import {
  createRecordPageViewWorkflow,
  createGetTotalPageViewsWorkflow,
  createGetPageViewTrendWorkflow,
  createGetReferrerRankingWorkflow,
  createGetDeviceDistributionWorkflow,
} from "@shared/workflows/analytics/page-view";
import { AnalyticsRepositoryProvider } from "../../infrastructure/analytics";
import { LoggerProvider } from "../../infrastructure/logger";

export const PageViewWorkflowProvider = {
  record: createRecordPageViewWorkflow(validatePageView)(
    AnalyticsRepositoryProvider.pageView.persist,
  )(LoggerProvider.console),

  getTotalPageViews: createGetTotalPageViewsWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.pageView.search,
  )(LoggerProvider.console),

  getPageViewTrend: createGetPageViewTrendWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.pageView.search,
  )(LoggerProvider.console),

  getReferrerRanking: createGetReferrerRankingWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.pageView.search,
  )(LoggerProvider.console),

  getDeviceDistribution: createGetDeviceDistributionWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.pageView.search,
  )(LoggerProvider.console),
} as const;
