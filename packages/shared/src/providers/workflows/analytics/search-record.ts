import { validateSearchRecord } from "@shared/domains/analytics/search-record";
import { validatePeriod } from "@shared/domains/analytics/common";
import {
  createRecordSearchWorkflow,
  createGetSearchCountWorkflow,
  createGetSearchKeywordRankingWorkflow,
  createGetSearchCountTrendWorkflow,
  createGetZeroHitKeywordsWorkflow,
} from "@shared/workflows/analytics/search-record";
import { AnalyticsRepositoryProvider } from "../../infrastructure/analytics";
import { LoggerProvider } from "../../infrastructure/logger";

export const SearchRecordWorkflowProvider = {
  record: createRecordSearchWorkflow(validateSearchRecord)(
    AnalyticsRepositoryProvider.searchRecord.persist,
  )(LoggerProvider.console),

  getSearchCount: createGetSearchCountWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.searchRecord.search,
  )(LoggerProvider.console),

  getSearchKeywordRanking: createGetSearchKeywordRankingWorkflow(
    validatePeriod,
  )(AnalyticsRepositoryProvider.searchRecord.search)(LoggerProvider.console),

  getSearchCountTrend: createGetSearchCountTrendWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.searchRecord.search,
  )(LoggerProvider.console),

  getZeroHitKeywords: createGetZeroHitKeywordsWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.searchRecord.search,
  )(LoggerProvider.console),
} as const;
