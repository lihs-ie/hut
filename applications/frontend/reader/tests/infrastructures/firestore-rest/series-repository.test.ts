/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { createFirestoreRestSeriesRepository } from "@/infrastructures/firestore-rest/series-repository";
import type { FirestoreRestClient } from "@/infrastructures/firestore-rest/client";
import {
  isAggregateNotFoundError,
  isUnexpectedError,
} from "@shared/aspects/error";
import {
  validateCriteria,
  validateSeriesIdentifier,
  type Series,
} from "@shared/domains/series";
import { validateSlug, PublishStatus } from "@shared/domains/common";

const createStubClient = (
  overrides: Partial<FirestoreRestClient> = {},
): FirestoreRestClient => ({
  getDocument: overrides.getDocument ?? (async () => null),
  runQuery: overrides.runQuery ?? (async () => []),
});

const sampleSeriesFields = () => ({
  identifier: "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
  title: "連載タイトル",
  slug: "sample-series",
  tags: [],
  subTitle: "副題",
  description: "連載の説明",
  cover: "https://example.com/cover.png",
  chapters: [],
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

describe("infrastructures/firestore-rest/series-repository", () => {
  describe("find", () => {
    it("連載を取得して返す", async () => {
      const getDocument = vi.fn().mockResolvedValue(sampleSeriesFields());
      const repository = createFirestoreRestSeriesRepository(
        createStubClient({ getDocument }),
      );
      const identifier = validateSeriesIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();

      const outcome = await collect(() => repository.find(identifier));

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value.title).toBe("連載タイトル");
        expect(outcome.value.subTitle).toBe("副題");
        expect(outcome.value.cover).toBe("https://example.com/cover.png");
      }
      expect(getDocument).toHaveBeenCalledWith(
        "series/01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      );
    });

    it("見つからない場合は AggregateNotFoundError を返す", async () => {
      const repository = createFirestoreRestSeriesRepository(
        createStubClient({ getDocument: async () => null }),
      );
      const identifier = validateSeriesIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3V99",
      ).unwrap();

      const outcome = await collect(() => repository.find(identifier));

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isAggregateNotFoundError(outcome.error)).toBe(true);
      }
    });
  });

  describe("findBySlug", () => {
    it("slug で連載を検索する", async () => {
      const runQuery = vi.fn().mockResolvedValue([sampleSeriesFields()]);
      const repository = createFirestoreRestSeriesRepository(
        createStubClient({ runQuery }),
      );
      const slug = validateSlug("sample-series").unwrap();

      const outcome = await collect(() => repository.findBySlug(slug));

      expect(outcome.ok).toBe(true);
      expect(runQuery).toHaveBeenCalledWith(
        expect.objectContaining({
          collectionId: "series",
          where: [
            {
              field: "slug",
              op: "EQUAL",
              value: { stringValue: "sample-series" },
            },
          ],
        }),
      );
    });

    it("見つからない場合は AggregateNotFoundError を返す", async () => {
      const repository = createFirestoreRestSeriesRepository(
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

  describe("search", () => {
    it("status で絞り込み検索する", async () => {
      const runQuery = vi.fn().mockResolvedValue([sampleSeriesFields()]);
      const repository = createFirestoreRestSeriesRepository(
        createStubClient({ runQuery }),
      );
      const criteria = validateCriteria({
        slug: null,
        status: PublishStatus.PUBLISHED,
        tags: null,
        freeWord: null,
      }).unwrap();

      const outcome = await collect<Series[], unknown>(() =>
        repository.search(criteria),
      );

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value).toHaveLength(1);
      }
    });

    it("クライアントエラー時は UnexpectedError を返す", async () => {
      const repository = createFirestoreRestSeriesRepository(
        createStubClient({
          runQuery: async () => {
            throw new Error("fail");
          },
        }),
      );
      const criteria = validateCriteria({
        slug: null,
        status: null,
        tags: null,
        freeWord: null,
      }).unwrap();

      const outcome = await collect<Series[], unknown>(() =>
        repository.search(criteria),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });
  });

  describe("persist/terminate", () => {
    it("persist は読み取り専用エラーを返す", async () => {
      const repository = createFirestoreRestSeriesRepository(createStubClient());

      const outcome = await collect<void, unknown>(() =>
        repository.persist({} as never),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });

    it("terminate は読み取り専用エラーを返す", async () => {
      const repository = createFirestoreRestSeriesRepository(createStubClient());
      const identifier = validateSeriesIdentifier(
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
