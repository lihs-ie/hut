import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createArticleCreatedEvent,
  createArticleEditedEvent,
  createArticleTerminatedEvent,
} from "@shared/domains/articles/event";
import { toSnapshot } from "@shared/domains/articles";
import {
  ArticleMold,
  ArticleIdentifierMold,
} from "../../support/molds/domains/article";

describe("domains/articles/event", () => {
  describe("createArticleCreatedEvent", () => {
    it("typeがarticle.createdのイベントを返す", () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const snapshot = toSnapshot(article);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event = createArticleCreatedEvent(snapshot, occurredAt);

      expect(event.type).toBe("article.created");
    });

    it("payloadにsnapshotが設定される", () => {
      const article = Forger(ArticleMold).forgeWithSeed(2);
      const snapshot = toSnapshot(article);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event = createArticleCreatedEvent(snapshot, occurredAt);

      expect(event.payload.snapshot).toBe(snapshot);
    });

    it("occurredAtが設定される", () => {
      const article = Forger(ArticleMold).forgeWithSeed(3);
      const snapshot = toSnapshot(article);
      const occurredAt = new Date("2024-06-15T12:30:00Z");

      const event = createArticleCreatedEvent(snapshot, occurredAt);

      expect(event.occurredAt).toBe(occurredAt);
    });

    it("identifierがULID形式で生成される", () => {
      const article = Forger(ArticleMold).forgeWithSeed(4);
      const snapshot = toSnapshot(article);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event = createArticleCreatedEvent(snapshot, occurredAt);

      expect(event.identifier).toMatch(/^[0-9A-Z]{26}$/);
    });

    it("呼び出しごとに異なるidentifierが生成される", () => {
      const article = Forger(ArticleMold).forgeWithSeed(5);
      const snapshot = toSnapshot(article);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event1 = createArticleCreatedEvent(snapshot, occurredAt);
      const event2 = createArticleCreatedEvent(snapshot, occurredAt);

      expect(event1.identifier).not.toBe(event2.identifier);
    });
  });

  describe("createArticleEditedEvent", () => {
    it("typeがarticle.editedのイベントを返す", () => {
      const nextArticle = Forger(ArticleMold).forgeWithSeed(10);
      const beforeArticle = Forger(ArticleMold).forgeWithSeed(11);
      const nextSnapshot = toSnapshot(nextArticle);
      const beforeSnapshot = toSnapshot(beforeArticle);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event = createArticleEditedEvent(nextSnapshot, beforeSnapshot, occurredAt);

      expect(event.type).toBe("article.edited");
    });

    it("payloadにnextが設定される", () => {
      const nextArticle = Forger(ArticleMold).forgeWithSeed(12);
      const beforeArticle = Forger(ArticleMold).forgeWithSeed(13);
      const nextSnapshot = toSnapshot(nextArticle);
      const beforeSnapshot = toSnapshot(beforeArticle);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event = createArticleEditedEvent(nextSnapshot, beforeSnapshot, occurredAt);

      expect(event.payload.next).toBe(nextSnapshot);
    });

    it("payloadにbeforeが設定される", () => {
      const nextArticle = Forger(ArticleMold).forgeWithSeed(14);
      const beforeArticle = Forger(ArticleMold).forgeWithSeed(15);
      const nextSnapshot = toSnapshot(nextArticle);
      const beforeSnapshot = toSnapshot(beforeArticle);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event = createArticleEditedEvent(nextSnapshot, beforeSnapshot, occurredAt);

      expect(event.payload.before).toBe(beforeSnapshot);
    });

    it("occurredAtが設定される", () => {
      const nextArticle = Forger(ArticleMold).forgeWithSeed(16);
      const beforeArticle = Forger(ArticleMold).forgeWithSeed(17);
      const nextSnapshot = toSnapshot(nextArticle);
      const beforeSnapshot = toSnapshot(beforeArticle);
      const occurredAt = new Date("2024-09-20T08:00:00Z");

      const event = createArticleEditedEvent(nextSnapshot, beforeSnapshot, occurredAt);

      expect(event.occurredAt).toBe(occurredAt);
    });

    it("identifierがULID形式で生成される", () => {
      const nextArticle = Forger(ArticleMold).forgeWithSeed(18);
      const beforeArticle = Forger(ArticleMold).forgeWithSeed(19);
      const nextSnapshot = toSnapshot(nextArticle);
      const beforeSnapshot = toSnapshot(beforeArticle);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event = createArticleEditedEvent(nextSnapshot, beforeSnapshot, occurredAt);

      expect(event.identifier).toMatch(/^[0-9A-Z]{26}$/);
    });

    it("呼び出しごとに異なるidentifierが生成される", () => {
      const nextArticle = Forger(ArticleMold).forgeWithSeed(20);
      const beforeArticle = Forger(ArticleMold).forgeWithSeed(21);
      const nextSnapshot = toSnapshot(nextArticle);
      const beforeSnapshot = toSnapshot(beforeArticle);
      const occurredAt = new Date("2024-01-01T00:00:00Z");

      const event1 = createArticleEditedEvent(nextSnapshot, beforeSnapshot, occurredAt);
      const event2 = createArticleEditedEvent(nextSnapshot, beforeSnapshot, occurredAt);

      expect(event1.identifier).not.toBe(event2.identifier);
    });
  });

  describe("createArticleTerminatedEvent", () => {
    it("typeがarticle.terminatedのイベントを返す", () => {
      const articleIdentifier = Forger(ArticleIdentifierMold).forgeWithSeed(30);

      const event = createArticleTerminatedEvent(articleIdentifier);

      expect(event.type).toBe("article.terminated");
    });

    it("payloadのarticleに識別子が設定される", () => {
      const articleIdentifier = Forger(ArticleIdentifierMold).forgeWithSeed(31);

      const event = createArticleTerminatedEvent(articleIdentifier);

      expect(event.payload.article).toBe(articleIdentifier);
    });

    it("occurredAtが設定される", () => {
      const articleIdentifier = Forger(ArticleIdentifierMold).forgeWithSeed(32);
      const before = new Date();

      const event = createArticleTerminatedEvent(articleIdentifier);

      const after = new Date();
      expect(event.occurredAt.getTime()).toBeGreaterThanOrEqual(before.getTime());
      expect(event.occurredAt.getTime()).toBeLessThanOrEqual(after.getTime());
    });

    it("identifierがULID形式で生成される", () => {
      const articleIdentifier = Forger(ArticleIdentifierMold).forgeWithSeed(33);

      const event = createArticleTerminatedEvent(articleIdentifier);

      expect(event.identifier).toMatch(/^[0-9A-Z]{26}$/);
    });

    it("呼び出しごとに異なるidentifierが生成される", () => {
      const articleIdentifier = Forger(ArticleIdentifierMold).forgeWithSeed(34);

      const event1 = createArticleTerminatedEvent(articleIdentifier);
      const event2 = createArticleTerminatedEvent(articleIdentifier);

      expect(event1.identifier).not.toBe(event2.identifier);
    });
  });
});
