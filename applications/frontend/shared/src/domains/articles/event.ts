import { ulid } from "ulid";
import { Event } from "../common";
import { ArticleIdentifier, ArticleSnapshot } from "./common";

export type ArticleCreatedEvent = Event<
  "article.created",
  {
    snapshot: ArticleSnapshot;
  }
>;

export const createArticleCreatedEvent = (
  article: ArticleSnapshot,
  occurredAt: Date,
): ArticleCreatedEvent => ({
  identifier: ulid(),
  type: "article.created",
  occurredAt,
  payload: {
    snapshot: article,
  },
});

export type ArticleEditedEvent = Event<
  "article.edited",
  {
    next: ArticleSnapshot;
    before: ArticleSnapshot;
  }
>;

export const createArticleEditedEvent = (
  next: ArticleSnapshot,
  before: ArticleSnapshot,
  occurredAt: Date,
): ArticleEditedEvent => ({
  identifier: ulid(),
  type: "article.edited",
  occurredAt,
  payload: {
    next,
    before,
  },
});

export type ArticleTerminatedEvent = Event<
  "article.terminated",
  {
    article: ArticleIdentifier;
  }
>;

export const createArticleTerminatedEvent = (
  article: ArticleIdentifier,
): ArticleTerminatedEvent => ({
  identifier: ulid(),
  type: "article.terminated",
  occurredAt: new Date(),
  payload: {
    article,
  },
});

export type ArticleEvent =
  | ArticleCreatedEvent
  | ArticleEditedEvent
  | ArticleTerminatedEvent;
