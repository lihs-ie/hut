import { validatePeriod } from "@shared/domains/analytics/common";
import {
  createGetContentRankingWorkflow,
  createGetTagPageViewsWorkflow,
  createGetContentTypeComparisonWorkflow,
} from "@shared/workflows/analytics/content";
import { AnalyticsRepositoryProvider } from "../../infrastructure/analytics";
import { ArticleRepositoryProvider } from "../../infrastructure/articles";
import { MemoRepositoryProvider } from "../../infrastructure/memo";
import { LoggerProvider } from "../../infrastructure/logger";

export const ContentWorkflowProvider = {
  getContentRanking: createGetContentRankingWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.pageView.search,
  )(ArticleRepositoryProvider.firebase.search)(
    MemoRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  getTagPageViews: createGetTagPageViewsWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.pageView.search,
  )(ArticleRepositoryProvider.firebase.search)(LoggerProvider.console),

  getContentTypeComparison: createGetContentTypeComparisonWorkflow(
    validatePeriod,
  )(AnalyticsRepositoryProvider.pageView.search)(LoggerProvider.console),
} as const;
