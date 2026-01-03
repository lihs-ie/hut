import { ulid } from "ulid";
import { Event } from "../common";
import { ArticleIdentifier } from "./common";

export type ArticleViewedEvent = Event<{
  article: ArticleIdentifier;
}>;

export const createArticleViewedEvent = (
  article: ArticleIdentifier
): ArticleViewedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    article,
  },
});

export type ArticleDraftedEvent = Event<{
  article: ArticleIdentifier;
}>;

export const createArticleDraftedEvent = (
  article: ArticleIdentifier
): ArticleDraftedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    article,
  },
});

export type ArticlePublishedEvent = Event<{
  article: ArticleIdentifier;
}>;

export const createArticlePublishedEvent = (
  article: ArticleIdentifier
): ArticlePublishedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    article,
  },
});

export type ArticleUpdatedEvent = Event<{
  article: ArticleIdentifier;
}>;

export const createArticleUpdatedEvent = (
  article: ArticleIdentifier
): ArticleUpdatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    article,
  },
});

export type ArticleArchivedEvent = Event<{
  article: ArticleIdentifier;
}>;

export const createArticleArchivedEvent = (
  article: ArticleIdentifier
): ArticleArchivedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    article,
  },
});

export type ArticleTerminatedEvent = Event<{
  article: ArticleIdentifier;
}>;

export const createArticleTerminatedEvent = (
  article: ArticleIdentifier
): ArticleTerminatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  payload: {
    article,
  },
});

export type ArticleEvent =
  | ArticleViewedEvent
  | ArticleDraftedEvent
  | ArticlePublishedEvent
  | ArticleUpdatedEvent
  | ArticleArchivedEvent
  | ArticleTerminatedEvent;
