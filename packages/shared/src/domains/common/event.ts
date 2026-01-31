import { UnexpectedError } from "@shared/aspects/error";
import { AsyncResult } from "@shared/aspects/result";

export type EventType =
  | "memo.created"
  | "memo.edited"
  | "memo.terminated"
  | "article.created"
  | "article.edited"
  | "article.terminated"
  | "series.persisted"
  | "series.terminated"
  | "tag.persisted"
  | "tag.terminated";

export type Event<T extends EventType, P> = {
  identifier: string;
  occurredAt: Date;
  type: T;
  payload: P;
};

export interface EventBroker {
  publish<T extends EventType, P>(
    event: Event<T, P>,
  ): AsyncResult<void, UnexpectedError>;
}
