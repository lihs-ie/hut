import { cache } from "react";
import {
  validatePeriod,
  resolveDateRange,
  resolvePreviousDateRange,
} from "@shared/domains/analytics/common";
import { criteriaSchema as pageViewCriteriaSchema } from "@shared/domains/analytics/page-view";
import { validateCriteria as validateEngagementCriteria } from "@shared/domains/analytics/engagement";
import { validateCriteria as validateUniqueVisitorCriteria } from "@shared/domains/analytics/unique-visitor";
import { validateCriteria as validateSearchRecordCriteria } from "@shared/domains/analytics/search-record";
import { AnalyticsRepositoryProvider } from "@shared/providers/infrastructure/analytics";
import { ArticleRepositoryProvider } from "@shared/providers/infrastructure/articles";
import { MemoRepositoryProvider } from "@shared/providers/infrastructure/memo";
import { TagRepositoryProvider } from "@shared/providers/infrastructure/tag";
import {
  buildEmptyArticleCriteria,
  buildEmptyMemoCriteria,
} from "@shared/workflows/analytics/title-resolution";
import { validateCriteria as validateTagCriteria } from "@shared/domains/attributes/tag";

export const loadCurrentPageViews = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolveDateRange(validatedPeriod);
  const criteria = pageViewCriteriaSchema.parse({ dateRange });
  return AnalyticsRepositoryProvider.pageView.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadPreviousPageViews = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolvePreviousDateRange(validatedPeriod);
  const criteria = pageViewCriteriaSchema.parse({ dateRange });
  return AnalyticsRepositoryProvider.pageView.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadCurrentEngagement = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolveDateRange(validatedPeriod);
  const criteria = validateEngagementCriteria({ dateRange }).unwrap();
  return AnalyticsRepositoryProvider.engagementRecord.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadPreviousEngagement = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolvePreviousDateRange(validatedPeriod);
  const criteria = validateEngagementCriteria({ dateRange }).unwrap();
  return AnalyticsRepositoryProvider.engagementRecord.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadCurrentUniqueVisitors = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolveDateRange(validatedPeriod);
  const criteria = validateUniqueVisitorCriteria({ dateRange }).unwrap();
  return AnalyticsRepositoryProvider.uniqueVisitor.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadPreviousUniqueVisitors = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolvePreviousDateRange(validatedPeriod);
  const criteria = validateUniqueVisitorCriteria({ dateRange }).unwrap();
  return AnalyticsRepositoryProvider.uniqueVisitor.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadCurrentSearchRecords = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolveDateRange(validatedPeriod);
  const criteria = validateSearchRecordCriteria({ dateRange }).unwrap();
  return AnalyticsRepositoryProvider.searchRecord.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadPreviousSearchRecords = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolvePreviousDateRange(validatedPeriod);
  const criteria = validateSearchRecordCriteria({ dateRange }).unwrap();
  return AnalyticsRepositoryProvider.searchRecord.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadZeroHitSearchRecords = cache(async (period: string) => {
  const validatedPeriod = validatePeriod(period).unwrap();
  const dateRange = resolveDateRange(validatedPeriod);
  const criteria = validateSearchRecordCriteria({
    dateRange,
    hasResults: false,
  }).unwrap();
  return AnalyticsRepositoryProvider.searchRecord.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadAllArticles = cache(async () => {
  const criteria = buildEmptyArticleCriteria();
  return ArticleRepositoryProvider.firebase.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadAllMemos = cache(async () => {
  const criteria = buildEmptyMemoCriteria();
  return MemoRepositoryProvider.firebase.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});

export const loadAllTags = cache(async () => {
  const criteria = validateTagCriteria({ name: null }).unwrap();
  return TagRepositoryProvider.firebase.search(criteria).match({
    ok: (result) => result,
    err: (error) => {
      throw error;
    },
  });
});
