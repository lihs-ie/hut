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
): ArticleCreatedEvent =>
  ({
    identifier: ulid(),
    occurredAt,
    payload: {
      snapshot: article,
    },
  }) as ArticleCreatedEvent;

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
): ArticleEditedEvent =>
  ({
    identifier: ulid(),
    occurredAt,
    payload: {
      next,
      before,
    },
  }) as ArticleEditedEvent;

export type ArticleTerminatedEvent = Event<
  "article.terminated",
  {
    article: ArticleIdentifier;
  }
>;

export const createArticleTerminatedEvent = (
  article: ArticleIdentifier,
): ArticleTerminatedEvent =>
  ({
    identifier: ulid(),
    occurredAt: new Date(),
    payload: {
      article,
    },
  }) as ArticleTerminatedEvent;

export type ArticleEvent =
  | ArticleCreatedEvent
  | ArticleEditedEvent
  | ArticleTerminatedEvent;
