/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { createFirestoreRestArticleRepository } from "@/infrastructures/firestore-rest/article-repository";
import type { FirestoreRestClient } from "@/infrastructures/firestore-rest/client";
import {
  isAggregateNotFoundError,
  isUnexpectedError,
} from "@shared/aspects/error";
import {
  validateArticleIdentifier,
  validateCriteria,
  type Article,
} from "@shared/domains/articles";
import { validateSlug, PublishStatus } from "@shared/domains/common";

const createStubClient = (
  overrides: Partial<FirestoreRestClient> = {},
): FirestoreRestClient => ({
  getDocument: overrides.getDocument ?? (async () => null),
  runQuery: overrides.runQuery ?? (async () => []),
});

const sampleArticleFields = () => ({
  identifier: "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
  title: "サンプル記事",
  content: "本文です",
  excerpt: "抜粋",
  slug: "sample-article",
  status: "published",
  tags: [],
  images: [],
  timeline: {
    createdAt: "2024-01-15T10:30:00Z",
    updatedAt: "2024-02-01T12:00:00Z",
  },
});

type Outcome<T, E> = { ok: true; value: T } | { ok: false; error: E };

const collect = async <T, E>(
  runner: () => { match: (handlers: {
    ok: (value: T) => Outcome<T, E>;
    err: (error: E) => Outcome<T, E>;
  }) => Promise<Outcome<T, E>> },
): Promise<Outcome<T, E>> =>
  runner().match<Outcome<T, E>>({
    ok: (value: T) => ({ ok: true, value }),
    err: (error: E) => ({ ok: false, error }),
  });

describe("infrastructures/firestore-rest/article-repository", () => {
  describe("find", () => {
    it("指定した identifier のドキュメントを取得して Article に変換する", async () => {
      const getDocument = vi.fn().mockResolvedValue(sampleArticleFields());
      const client = createStubClient({ getDocument });
      const repository = createFirestoreRestArticleRepository(client);
      const identifier = validateArticleIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();

      const outcome = await collect(() => repository.find(identifier));

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value.identifier).toBe("01HCW7Z5YFR3RGTKJN4J5Y3VCR");
        expect(outcome.value.title).toBe("サンプル記事");
      }
      expect(getDocument).toHaveBeenCalledWith(
        "articles/01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      );
    });

    it("ドキュメントが存在しない場合は AggregateNotFoundError を返す", async () => {
      const client = createStubClient({
        getDocument: async () => null,
      });
      const repository = createFirestoreRestArticleRepository(client);
      const identifier = validateArticleIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3V99",
      ).unwrap();

      const outcome = await collect(() => repository.find(identifier));

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isAggregateNotFoundError(outcome.error)).toBe(true);
      }
    });

    it("クライアントが失敗した場合は UnexpectedError を返す", async () => {
      const client = createStubClient({
        getDocument: async () => {
          throw new Error("network down");
        },
      });
      const repository = createFirestoreRestArticleRepository(client);
      const identifier = validateArticleIdentifier(
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
    it("slug で検索して最初のドキュメントを返す", async () => {
      const runQuery = vi.fn().mockResolvedValue([sampleArticleFields()]);
      const client = createStubClient({ runQuery });
      const repository = createFirestoreRestArticleRepository(client);
      const slug = validateSlug("sample-article").unwrap();

      const outcome = await collect(() => repository.findBySlug(slug));

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value.slug).toBe("sample-article");
      }
      expect(runQuery).toHaveBeenCalledWith(
        expect.objectContaining({
          collectionId: "articles",
          where: [
            {
              field: "slug",
              op: "EQUAL",
              value: { stringValue: "sample-article" },
            },
          ],
        }),
      );
    });

    it("一致するドキュメントが無い場合は AggregateNotFoundError を返す", async () => {
      const client = createStubClient({ runQuery: async () => [] });
      const repository = createFirestoreRestArticleRepository(client);
      const slug = validateSlug("missing").unwrap();

      const outcome = await collect(() => repository.findBySlug(slug));

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isAggregateNotFoundError(outcome.error)).toBe(true);
      }
    });
  });

  describe("ofIdentifiers", () => {
    it("identifier ごとにドキュメントを取得して配列で返す", async () => {
      const firstFields = sampleArticleFields();
      const secondFields = {
        ...sampleArticleFields(),
        identifier: "01HCW7Z5YFR3RGTKJN4J5Y3VCS",
        slug: "second-article",
      };
      const getDocument = vi
        .fn()
        .mockResolvedValueOnce(firstFields)
        .mockResolvedValueOnce(secondFields);
      const client = createStubClient({ getDocument });
      const repository = createFirestoreRestArticleRepository(client);
      const first = validateArticleIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();
      const second = validateArticleIdentifier(
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

    it("ドキュメントが見つからない場合はスキップする", async () => {
      const getDocument = vi
        .fn()
        .mockResolvedValueOnce(sampleArticleFields())
        .mockResolvedValueOnce(null);
      const client = createStubClient({ getDocument });
      const repository = createFirestoreRestArticleRepository(client);
      const first = validateArticleIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();
      const second = validateArticleIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCS",
      ).unwrap();

      const outcome = await collect(() =>
        repository.ofIdentifiers([first, second]),
      );

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value).toHaveLength(1);
      }
    });

    it("throwOnMissing=true でドキュメントが無い場合は AggregateNotFoundError を返す", async () => {
      const client = createStubClient({
        getDocument: async () => null,
      });
      const repository = createFirestoreRestArticleRepository(client);
      const identifier = validateArticleIdentifier(
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

  describe("search", () => {
    it("公開ステータスで絞り込んで記事を取得する", async () => {
      const runQuery = vi.fn().mockResolvedValue([sampleArticleFields()]);
      const client = createStubClient({ runQuery });
      const repository = createFirestoreRestArticleRepository(client);
      const criteria = validateCriteria({
        status: "published",
        slug: null,
        tags: null,
        freeWord: null,
      }).unwrap();

      const outcome = await collect<Article[], unknown>(() =>
        repository.search(criteria),
      );

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value).toHaveLength(1);
      }
      const call = runQuery.mock.calls[0][0] as {
        where: Array<{ field: string; op: string }>;
      };
      const statusFilter = call.where.find(
        (filter) => filter.field === "status",
      );
      expect(statusFilter).toBeDefined();
    });

    it("freeWord はクライアント側でフィルタリングする", async () => {
      const first = sampleArticleFields();
      const second = {
        ...sampleArticleFields(),
        identifier: "01HCW7Z5YFR3RGTKJN4J5Y3VCS",
        slug: "second",
        title: "別のタイトル",
        content: "まったく違う本文",
        excerpt: "まったく違う抜粋",
      };
      const runQuery = vi.fn().mockResolvedValue([first, second]);
      const client = createStubClient({ runQuery });
      const repository = createFirestoreRestArticleRepository(client);
      const criteria = validateCriteria({
        status: PublishStatus.PUBLISHED,
        slug: null,
        tags: null,
        freeWord: "サンプル",
      }).unwrap();

      const outcome = await collect<Article[], unknown>(() =>
        repository.search(criteria),
      );

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value).toHaveLength(1);
        expect(outcome.value[0].title).toBe("サンプル記事");
      }
    });

    it("クライアントエラーは UnexpectedError に変換される", async () => {
      const client = createStubClient({
        runQuery: async () => {
          throw new Error("backend down");
        },
      });
      const repository = createFirestoreRestArticleRepository(client);
      const criteria = validateCriteria({
        status: null,
        slug: null,
        tags: null,
        freeWord: null,
      }).unwrap();

      const outcome = await collect<Article[], unknown>(() =>
        repository.search(criteria),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });
  });

  describe("persist/terminate", () => {
    it("persist は UnexpectedError を返す (read-only)", async () => {
      const client = createStubClient();
      const repository = createFirestoreRestArticleRepository(client);

      const outcome = await collect<void, unknown>(() =>
        repository.persist({} as never),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });

    it("terminate は UnexpectedError を返す (read-only)", async () => {
      const client = createStubClient();
      const repository = createFirestoreRestArticleRepository(client);
      const identifier = validateArticleIdentifier(
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
