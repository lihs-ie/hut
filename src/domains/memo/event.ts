import { ulid } from "ulid";
import { Event } from "../common";
import { MemoIdentifier } from "./common";

export type MemoCreatedEvent = Event<{
  memo: MemoIdentifier;
}>;

export const createMemoCreatedEvent = (
  memo: MemoIdentifier
): MemoCreatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    memo,
  },
});

export type MemoUpdatedEvent = Event<{
  memo: MemoIdentifier;
}>;

export const createMemoUpdatedEvent = (
  memo: MemoIdentifier
): MemoUpdatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    memo,
  },
});

export type MemoTerminatedEvent = Event<{
  memo: MemoIdentifier;
}>;

export const createMemoTerminatedEvent = (
  memo: MemoIdentifier
): MemoTerminatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    memo,
  },
});

export type MemoEvent =
  | MemoCreatedEvent
  | MemoUpdatedEvent
  | MemoTerminatedEvent;
