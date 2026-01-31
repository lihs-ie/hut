import { ulid } from "ulid";
import { Event } from "../common";
import { SeriesIdentifier } from "./common";

export type SeriesCreatedEvent = Event<{
  series: SeriesIdentifier;
}>;

export const createSeriesCreatedEvent = (
  series: SeriesIdentifier
): SeriesCreatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    series,
  },
});

export type SeriesUpdatedEvent = Event<{
  series: SeriesIdentifier;
}>;

export const createSeriesUpdatedEvent = (
  series: SeriesIdentifier
): SeriesUpdatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    series,
  },
});

export type SeriesTerminatedEvent = Event<{
  series: SeriesIdentifier;
}>;

export const createSeriesTerminatedEvent = (
  series: SeriesIdentifier
): SeriesTerminatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    series,
  },
});

export type SeriesEvent =
  | SeriesCreatedEvent
  | SeriesUpdatedEvent
  | SeriesTerminatedEvent;
