"use server";

import {
  loadCurrentPageViews,
  loadPreviousPageViews,
  loadCurrentUniqueVisitors,
  loadPreviousUniqueVisitors,
  loadCurrentEngagement,
  loadPreviousEngagement,
  loadCurrentSearchRecords,
  loadPreviousSearchRecords,
  loadZeroHitSearchRecords,
  loadAllArticles,
  loadAllMemos,
} from "./analytics/loader";
import { validatePeriodComparison } from "@shared/domains/analytics/common";
import type {
  PeriodComparison,
  RankedItem,
  Distribution,
  TrendPoint,
} from "@shared/domains/analytics/common";
import {
  aggregateByDate,
  aggregateByReferrer,
  aggregateByDevice,
} from "@shared/workflows/analytics/page-view";
import {
  aggregateByContent,
  aggregateByContentType,
  aggregateByTag,
  buildArticleTagMap,
} from "@shared/workflows/analytics/content";
import {
  calculateAverageDwellTime,
  aggregateDwellTimeByContent,
  aggregateScrollDepth,
} from "@shared/workflows/analytics/engagement";
import {
  aggregateByKeyword,
  aggregateSearchByDate,
} from "@shared/workflows/analytics/search-record";
import {
  buildContentTitleMap,
  resolveRankedItemTitles,
} from "@shared/workflows/analytics/title-resolution";

export async function getTotalPageViews(
  period: string,
): Promise<PeriodComparison> {
  const [current, previous] = await Promise.all([
    loadCurrentPageViews(period),
    loadPreviousPageViews(period),
  ]);
  return validatePeriodComparison({
    current: current.length,
    previous: previous.length,
  }).unwrap();
}

export async function getPageViewTrend(period: string): Promise<TrendPoint[]> {
  const pageViews = await loadCurrentPageViews(period);
  return aggregateByDate(pageViews);
}

export async function getReferrerRanking(
  period: string,
): Promise<RankedItem[]> {
  const pageViews = await loadCurrentPageViews(period);
  return aggregateByReferrer(pageViews);
}

export async function getDeviceDistribution(
  period: string,
): Promise<Distribution[]> {
  const pageViews = await loadCurrentPageViews(period);
  return aggregateByDevice(pageViews);
}

export async function getUniqueVisitors(
  period: string,
): Promise<PeriodComparison> {
  const [current, previous] = await Promise.all([
    loadCurrentUniqueVisitors(period),
    loadPreviousUniqueVisitors(period),
  ]);
  return validatePeriodComparison({
    current: current.length,
    previous: previous.length,
  }).unwrap();
}

export async function getAverageDwellTime(
  period: string,
): Promise<PeriodComparison> {
  const [current, previous] = await Promise.all([
    loadCurrentEngagement(period),
    loadPreviousEngagement(period),
  ]);
  return validatePeriodComparison({
    current: calculateAverageDwellTime(current),
    previous: calculateAverageDwellTime(previous),
  }).unwrap();
}

export async function getDwellTimeRanking(
  period: string,
): Promise<RankedItem[]> {
  const [records, articles, memos] = await Promise.all([
    loadCurrentEngagement(period),
    loadAllArticles(),
    loadAllMemos(),
  ]);
  const ranking = aggregateDwellTimeByContent(records);
  const titleMap = buildContentTitleMap(articles, memos);
  return resolveRankedItemTitles(ranking, titleMap);
}

export async function getScrollDepthDistribution(
  period: string,
): Promise<Distribution[]> {
  const records = await loadCurrentEngagement(period);
  return aggregateScrollDepth(records);
}

export async function getSearchCount(
  period: string,
): Promise<PeriodComparison> {
  const [current, previous] = await Promise.all([
    loadCurrentSearchRecords(period),
    loadPreviousSearchRecords(period),
  ]);
  return validatePeriodComparison({
    current: current.length,
    previous: previous.length,
  }).unwrap();
}

export async function getSearchKeywordRanking(
  period: string,
): Promise<RankedItem[]> {
  const records = await loadCurrentSearchRecords(period);
  return aggregateByKeyword(records);
}

export async function getSearchCountTrend(
  period: string,
): Promise<TrendPoint[]> {
  const records = await loadCurrentSearchRecords(period);
  return aggregateSearchByDate(records);
}

export async function getZeroHitKeywords(
  period: string,
): Promise<RankedItem[]> {
  const records = await loadZeroHitSearchRecords(period);
  return aggregateByKeyword(records);
}

export async function getContentRanking(period: string): Promise<RankedItem[]> {
  const [pageViews, articles, memos] = await Promise.all([
    loadCurrentPageViews(period),
    loadAllArticles(),
    loadAllMemos(),
  ]);
  const ranking = aggregateByContent(pageViews);
  const titleMap = buildContentTitleMap(articles, memos);
  return resolveRankedItemTitles(ranking, titleMap);
}

export async function getTagPageViews(period: string): Promise<RankedItem[]> {
  const [pageViews, articles] = await Promise.all([
    loadCurrentPageViews(period),
    loadAllArticles(),
  ]);
  return aggregateByTag(pageViews, buildArticleTagMap(articles));
}

export async function getContentTypeComparison(
  period: string,
): Promise<Distribution[]> {
  const pageViews = await loadCurrentPageViews(period);
  return aggregateByContentType(pageViews);
}
