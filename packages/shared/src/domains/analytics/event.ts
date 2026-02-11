import { ulid } from "ulid";
import { Event } from "../common";
import { PageView } from "./page-view";
import { UniqueVisitor } from "./unique-visitor";
import { EngagementRecord } from "./engagement";
import { SearchRecord } from "./search-record";

export type PageViewRecordedEvent = Event<
  "analytics.pageView.recorded",
  { pageView: PageView }
>;

export const createPageViewRecordedEvent = (
  pageView: PageView,
  occurredAt: Date,
): PageViewRecordedEvent =>
  ({
    identifier: ulid(),
    occurredAt,
    type: "analytics.pageView.recorded",
    payload: { pageView },
  }) as PageViewRecordedEvent;

export type UniqueVisitorRecordedEvent = Event<
  "analytics.uniqueVisitor.recorded",
  { visitor: UniqueVisitor }
>;

export const createUniqueVisitorRecordedEvent = (
  visitor: UniqueVisitor,
  occurredAt: Date,
): UniqueVisitorRecordedEvent =>
  ({
    identifier: ulid(),
    occurredAt,
    type: "analytics.uniqueVisitor.recorded",
    payload: { visitor },
  }) as UniqueVisitorRecordedEvent;

export type EngagementRecordedEvent = Event<
  "analytics.engagement.recorded",
  { record: EngagementRecord }
>;

export const createEngagementRecordedEvent = (
  record: EngagementRecord,
  occurredAt: Date,
): EngagementRecordedEvent =>
  ({
    identifier: ulid(),
    occurredAt,
    type: "analytics.engagement.recorded",
    payload: { record },
  }) as EngagementRecordedEvent;

export type SearchRecordedEvent = Event<
  "analytics.search.recorded",
  { record: SearchRecord }
>;

export const createSearchRecordedEvent = (
  record: SearchRecord,
  occurredAt: Date,
): SearchRecordedEvent =>
  ({
    identifier: ulid(),
    occurredAt,
    type: "analytics.search.recorded",
    payload: { record },
  }) as SearchRecordedEvent;

export type AnalyticsEvent =
  | PageViewRecordedEvent
  | UniqueVisitorRecordedEvent
  | EngagementRecordedEvent
  | SearchRecordedEvent;
