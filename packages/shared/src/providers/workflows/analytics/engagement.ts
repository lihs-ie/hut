import { validateEngagementRecord } from "@shared/domains/analytics/engagement";
import { validatePeriod } from "@shared/domains/analytics/common";
import {
  createRecordEngagementWorkflow,
  createGetAverageDwellTimeWorkflow,
  createGetDwellTimeRankingWorkflow,
  createGetScrollDepthDistributionWorkflow,
} from "@shared/workflows/analytics/engagement";
import { AnalyticsRepositoryProvider } from "../../infrastructure/analytics";
import { ArticleRepositoryProvider } from "../../infrastructure/articles";
import { MemoRepositoryProvider } from "../../infrastructure/memo";
import { LoggerProvider } from "../../infrastructure/logger";

export const EngagementWorkflowProvider = {
  record: createRecordEngagementWorkflow(validateEngagementRecord)(
    AnalyticsRepositoryProvider.engagementRecord.persist,
  )(LoggerProvider.console),

  getAverageDwellTime: createGetAverageDwellTimeWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.engagementRecord.search,
  )(LoggerProvider.console),

  getDwellTimeRanking: createGetDwellTimeRankingWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.engagementRecord.search,
  )(ArticleRepositoryProvider.firebase.search)(
    MemoRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  getScrollDepthDistribution: createGetScrollDepthDistributionWorkflow(
    validatePeriod,
  )(AnalyticsRepositoryProvider.engagementRecord.search)(
    LoggerProvider.console,
  ),
} as const;
