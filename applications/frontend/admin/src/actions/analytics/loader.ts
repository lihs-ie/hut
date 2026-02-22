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
import { AdminAnalyticsRepositoryProvider } from "@/providers/infrastructure/analytics";
import { AdminArticleRepositoryProvider } from "@/providers/infrastructure/articles";
import { AdminMemoRepositoryProvider } from "@/providers/infrastructure/memo";
import { AdminTagRepositoryProvider } from "@/providers/infrastructure/tag";
import {
  buildEmptyArticleCriteria,
  buildEmptyMemoCriteria,
} from "@shared/workflows/analytics/title-resolution";
import { validateCriteria as validateTagCriteria } from "@shared/domains/attributes/tag";
import type { DateRange, Period } from "@shared/domains/analytics/common";
import type { AsyncResult } from "@shared/aspects/result";
import type { UnexpectedError } from "@shared/aspects/error";

const resolveDateRangeFromPeriod = (
  period: string,
  resolver: (period: Period) => DateRange,
): DateRange => {
  const validatedPeriod = validatePeriod(period).unwrap();
  return resolver(validatedPeriod);
};

const searchAndUnwrap = <T>(
  asyncResult: AsyncResult<T, UnexpectedError>,
): Promise<T> => asyncResult.unwrap();

export const loadCurrentPageViews = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolveDateRange);
  const criteria = pageViewCriteriaSchema.parse({ dateRange });
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.pageView.search(criteria));
});

export const loadPreviousPageViews = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolvePreviousDateRange);
  const criteria = pageViewCriteriaSchema.parse({ dateRange });
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.pageView.search(criteria));
});

export const loadCurrentEngagement = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolveDateRange);
  const criteria = validateEngagementCriteria({ dateRange }).unwrap();
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.engagementRecord.search(criteria));
});

export const loadPreviousEngagement = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolvePreviousDateRange);
  const criteria = validateEngagementCriteria({ dateRange }).unwrap();
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.engagementRecord.search(criteria));
});

export const loadCurrentUniqueVisitors = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolveDateRange);
  const criteria = validateUniqueVisitorCriteria({ dateRange }).unwrap();
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.uniqueVisitor.search(criteria));
});

export const loadPreviousUniqueVisitors = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolvePreviousDateRange);
  const criteria = validateUniqueVisitorCriteria({ dateRange }).unwrap();
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.uniqueVisitor.search(criteria));
});

export const loadCurrentSearchRecords = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolveDateRange);
  const criteria = validateSearchRecordCriteria({ dateRange }).unwrap();
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.searchRecord.search(criteria));
});

export const loadPreviousSearchRecords = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolvePreviousDateRange);
  const criteria = validateSearchRecordCriteria({ dateRange }).unwrap();
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.searchRecord.search(criteria));
});

export const loadZeroHitSearchRecords = cache(async (period: string) => {
  const dateRange = resolveDateRangeFromPeriod(period, resolveDateRange);
  const criteria = validateSearchRecordCriteria({
    dateRange,
    hasResults: false,
  }).unwrap();
  return searchAndUnwrap(AdminAnalyticsRepositoryProvider.searchRecord.search(criteria));
});

export const loadAllArticles = cache(async () => {
  const criteria = buildEmptyArticleCriteria();
  return searchAndUnwrap(AdminArticleRepositoryProvider.firebase.search(criteria));
});

export const loadAllMemos = cache(async () => {
  const criteria = buildEmptyMemoCriteria();
  return searchAndUnwrap(AdminMemoRepositoryProvider.firebase.search(criteria));
});

export const loadAllTags = cache(async () => {
  const criteria = validateTagCriteria({ name: null }).unwrap();
  return searchAndUnwrap(AdminTagRepositoryProvider.firebase.search(criteria));
});
