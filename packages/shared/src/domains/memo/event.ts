import { ulid } from "ulid";
import { Event } from "../common";
import { MemoIdentifier, MemoSnapshot } from "./common";

export type MemoEditedEvent = Event<
  "memo.edited",
  {
    next: MemoSnapshot;
    before: MemoSnapshot;
  }
>;

export const createMemoEditedEvent = (
  next: MemoSnapshot,
  before: MemoSnapshot,
  occurredAt: Date,
): MemoEditedEvent => {
  return {
    identifier: ulid(),
    occurredAt,
    type: "memo.edited",
    payload: {
      next,
      before,
    },
  };
};

export type MemoCreatedEvent = Event<
  "memo.created",
  {
    snapshot: MemoSnapshot;
  }
>;

export const createMemoCreatedEvent = (
  snapshot: MemoSnapshot,
  occurredAt: Date,
): MemoCreatedEvent => {
  return {
    identifier: ulid(),
    occurredAt,
    type: "memo.created",
    payload: {
      snapshot,
    },
  };
};

export type MemoTerminatedEvent = Event<
  "memo.terminated",
  {
    memo: MemoIdentifier;
  }
>;

export const createMemoTerminatedEvent = (
  memo: MemoIdentifier,
  occurredAt = new Date(),
): MemoTerminatedEvent => {
  return {
    identifier: ulid(),
    occurredAt,
    type: "memo.terminated",
    payload: {
      memo,
    },
  };
};

export type MemoEvent =
  | MemoCreatedEvent
  | MemoEditedEvent
  | MemoTerminatedEvent;
