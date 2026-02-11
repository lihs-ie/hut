import { ulid } from "ulid";
import { Event } from "../common";
import { SeriesIdentifier } from "./common";

type SeriesPayload = {
  series: SeriesIdentifier;
};

export type SeriesCreatedEvent = Event<"series.persisted", SeriesPayload>;

export const createSeriesCreatedEvent = (
  series: SeriesIdentifier
): SeriesCreatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  type: "series.persisted",
  payload: {
    series,
  },
});

export type SeriesUpdatedEvent = Event<"series.persisted", SeriesPayload>;

export const createSeriesUpdatedEvent = (
  series: SeriesIdentifier
): SeriesUpdatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  type: "series.persisted",
  payload: {
    series,
  },
});

export type SeriesTerminatedEvent = Event<"series.terminated", SeriesPayload>;

export const createSeriesTerminatedEvent = (
  series: SeriesIdentifier
): SeriesTerminatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  type: "series.terminated",
  payload: {
    series,
  },
});

export type SeriesEvent =
  | SeriesCreatedEvent
  | SeriesUpdatedEvent
  | SeriesTerminatedEvent;
