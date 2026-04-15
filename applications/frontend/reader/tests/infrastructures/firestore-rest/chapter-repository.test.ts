/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { createFirestoreRestChapterRepository } from "@/infrastructures/firestore-rest/chapter-repository";
import type { FirestoreRestClient } from "@/infrastructures/firestore-rest/client";
import {
  isAggregateNotFoundError,
  isUnexpectedError,
} from "@shared/aspects/error";
import { validateSlug } from "@shared/domains/common";
import { validateChapterIdentifier } from "@shared/domains/series/chapter";

const createStubClient = (
  overrides: Partial<FirestoreRestClient> = {},
): FirestoreRestClient => ({
  getDocument: overrides.getDocument ?? (async () => null),
  runQuery: overrides.runQuery ?? (async () => []),
});

const sampleChapterFields = () => ({
  identifier: "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
  title: "第1章",
  slug: "chapter-1",
  content: "章の本文",
  images: [],
  status: "published",
  timeline: {
    createdAt: "2024-01-15T10:30:00Z",
    updatedAt: "2024-02-01T12:00:00Z",
  },
});

type Outcome<T, E> = { ok: true; value: T } | { ok: false; error: E };

const collect = async <T, E>(
  runner: () => {
    match: (handlers: {
      ok: (value: T) => Outcome<T, E>;
      err: (error: E) => Outcome<T, E>;
    }) => Promise<Outcome<T, E>>;
  },
): Promise<Outcome<T, E>> =>
  runner().match<Outcome<T, E>>({
    ok: (value: T) => ({ ok: true, value }),
    err: (error: E) => ({ ok: false, error }),
  });

describe("infrastructures/firestore-rest/chapter-repository", () => {
  describe("find", () => {
    it("指定した identifier の章を返す", async () => {
      const getDocument = vi.fn().mockResolvedValue(sampleChapterFields());
      const repository = createFirestoreRestChapterRepository(
        createStubClient({ getDocument }),
      );
      const identifier = validateChapterIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();

      const outcome = await collect(() => repository.find(identifier));

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value.title).toBe("第1章");
        expect(outcome.value.slug).toBe("chapter-1");
      }
      expect(getDocument).toHaveBeenCalledWith(
        "chapters/01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      );
    });

    it("存在しない章は AggregateNotFoundError を返す", async () => {
      const repository = createFirestoreRestChapterRepository(
        createStubClient({ getDocument: async () => null }),
      );
      const identifier = validateChapterIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3V99",
      ).unwrap();

      const outcome = await collect(() => repository.find(identifier));

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isAggregateNotFoundError(outcome.error)).toBe(true);
      }
    });

    it("クライアント失敗時は UnexpectedError を返す", async () => {
      const repository = createFirestoreRestChapterRepository(
        createStubClient({
          getDocument: async () => {
            throw new Error("boom");
          },
        }),
      );
      const identifier = validateChapterIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();

      const outcome = await collect(() => repository.find(identifier));

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });
  });

  describe("findBySlug", () => {
    it("slug で章を検索する", async () => {
      const runQuery = vi.fn().mockResolvedValue([sampleChapterFields()]);
      const repository = createFirestoreRestChapterRepository(
        createStubClient({ runQuery }),
      );
      const slug = validateSlug("chapter-1").unwrap();

      const outcome = await collect(() => repository.findBySlug(slug));

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value.slug).toBe("chapter-1");
      }
      expect(runQuery).toHaveBeenCalledWith(
        expect.objectContaining({
          collectionId: "chapters",
          where: [
            {
              field: "slug",
              op: "EQUAL",
              value: { stringValue: "chapter-1" },
            },
          ],
        }),
      );
    });

    it("見つからない場合は AggregateNotFoundError を返す", async () => {
      const repository = createFirestoreRestChapterRepository(
        createStubClient({ runQuery: async () => [] }),
      );
      const slug = validateSlug("missing").unwrap();

      const outcome = await collect(() => repository.findBySlug(slug));

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isAggregateNotFoundError(outcome.error)).toBe(true);
      }
    });
  });

  describe("ofIdentifiers", () => {
    it("複数の identifier から章を取得する", async () => {
      const getDocument = vi
        .fn()
        .mockResolvedValueOnce(sampleChapterFields())
        .mockResolvedValueOnce({
          ...sampleChapterFields(),
          identifier: "01HCW7Z5YFR3RGTKJN4J5Y3VCS",
          slug: "chapter-2",
        });
      const repository = createFirestoreRestChapterRepository(
        createStubClient({ getDocument }),
      );
      const first = validateChapterIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();
      const second = validateChapterIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCS",
      ).unwrap();

      const outcome = await collect(() =>
        repository.ofIdentifiers([first, second]),
      );

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value).toHaveLength(2);
      }
    });

    it("throwOnMissing=true で見つからないと AggregateNotFoundError を返す", async () => {
      const repository = createFirestoreRestChapterRepository(
        createStubClient({ getDocument: async () => null }),
      );
      const identifier = validateChapterIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();

      const outcome = await collect(() =>
        repository.ofIdentifiers([identifier], true),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isAggregateNotFoundError(outcome.error)).toBe(true);
      }
    });
  });

  describe("persist/terminate", () => {
    it("persist は読み取り専用エラーを返す", async () => {
      const repository = createFirestoreRestChapterRepository(
        createStubClient(),
      );

      const outcome = await collect<void, unknown>(() =>
        repository.persist({} as never),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });

    it("terminate は読み取り専用エラーを返す", async () => {
      const repository = createFirestoreRestChapterRepository(
        createStubClient(),
      );
      const identifier = validateChapterIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();

      const outcome = await collect<void, unknown>(() =>
        repository.terminate(identifier),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });
  });
});
