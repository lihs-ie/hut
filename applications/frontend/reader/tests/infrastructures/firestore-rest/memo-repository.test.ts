/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { createFirestoreRestMemoRepository } from "@/infrastructures/firestore-rest/memo-repository";
import type { FirestoreRestClient } from "@/infrastructures/firestore-rest/client";
import {
  isAggregateNotFoundError,
  isUnexpectedError,
} from "@shared/aspects/error";
import {
  validateCriteria,
  validateMemoIdentifier,
  type Memo,
} from "@shared/domains/memo";
import { validateSlug, PublishStatus } from "@shared/domains/common";

const createStubClient = (
  overrides: Partial<FirestoreRestClient> = {},
): FirestoreRestClient => ({
  getDocument: overrides.getDocument ?? (async () => null),
  runQuery: overrides.runQuery ?? (async () => []),
});

const sampleMemoFields = () => ({
  identifier: "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
  title: "サンプルメモ",
  slug: "sample-memo",
  entries: [
    {
      text: "最初のエントリー",
      createdAt: "2024-01-15T10:30:00Z",
    },
  ],
  tags: [],
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

describe("infrastructures/firestore-rest/memo-repository", () => {
  describe("find", () => {
    it("メモを取得して entries を Date に変換する", async () => {
      const getDocument = vi.fn().mockResolvedValue(sampleMemoFields());
      const repository = createFirestoreRestMemoRepository(
        createStubClient({ getDocument }),
      );
      const identifier = validateMemoIdentifier(
        "01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      ).unwrap();

      const outcome = await collect(() => repository.find(identifier));

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value.title).toBe("サンプルメモ");
        expect(outcome.value.entries).toHaveLength(1);
        expect(outcome.value.entries[0].text).toBe("最初のエントリー");
        expect(outcome.value.entries[0].createdAt).toBeInstanceOf(Date);
      }
      expect(getDocument).toHaveBeenCalledWith(
        "memos/01HCW7Z5YFR3RGTKJN4J5Y3VCR",
      );
    });

    it("見つからない場合は AggregateNotFoundError を返す", async () => {
      const repository = createFirestoreRestMemoRepository(
        createStubClient({ getDocument: async () => null }),
      );
      const identifier = validateMemoIdentifier(
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
    it("slug でメモを検索する", async () => {
      const runQuery = vi.fn().mockResolvedValue([sampleMemoFields()]);
      const repository = createFirestoreRestMemoRepository(
        createStubClient({ runQuery }),
      );
      const slug = validateSlug("sample-memo").unwrap();

      const outcome = await collect(() => repository.findBySlug(slug));

      expect(outcome.ok).toBe(true);
      expect(runQuery).toHaveBeenCalledWith(
        expect.objectContaining({
          collectionId: "memos",
          where: [
            {
              field: "slug",
              op: "EQUAL",
              value: { stringValue: "sample-memo" },
            },
          ],
        }),
      );
    });
  });

  describe("search", () => {
    it("status で絞り込み検索する", async () => {
      const runQuery = vi.fn().mockResolvedValue([sampleMemoFields()]);
      const repository = createFirestoreRestMemoRepository(
        createStubClient({ runQuery }),
      );
      const criteria = validateCriteria({
        status: PublishStatus.PUBLISHED,
        tags: null,
        freeWord: null,
      }).unwrap();

      const outcome = await collect<Memo[], unknown>(() =>
        repository.search(criteria),
      );

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value).toHaveLength(1);
      }
      const call = runQuery.mock.calls[0][0] as {
        where: Array<{ field: string; op: string }>;
      };
      expect(
        call.where.some((filter) => filter.field === "status"),
      ).toBe(true);
    });

    it("freeWord はクライアント側でフィルタリングする", async () => {
      const first = sampleMemoFields();
      const second = {
        ...sampleMemoFields(),
        identifier: "01HCW7Z5YFR3RGTKJN4J5Y3VCS",
        slug: "different",
        title: "別のメモ",
        entries: [
          {
            text: "まったく違うテキスト",
            createdAt: "2024-03-01T00:00:00Z",
          },
        ],
      };
      const runQuery = vi.fn().mockResolvedValue([first, second]);
      const repository = createFirestoreRestMemoRepository(
        createStubClient({ runQuery }),
      );
      const criteria = validateCriteria({
        status: PublishStatus.PUBLISHED,
        tags: null,
        freeWord: "サンプル",
      }).unwrap();

      const outcome = await collect<Memo[], unknown>(() =>
        repository.search(criteria),
      );

      expect(outcome.ok).toBe(true);
      if (outcome.ok) {
        expect(outcome.value).toHaveLength(1);
        expect(outcome.value[0].title).toBe("サンプルメモ");
      }
    });

    it("クライアントエラー時は UnexpectedError を返す", async () => {
      const repository = createFirestoreRestMemoRepository(
        createStubClient({
          runQuery: async () => {
            throw new Error("fail");
          },
        }),
      );
      const criteria = validateCriteria({
        status: null,
        tags: null,
        freeWord: null,
      }).unwrap();

      const outcome = await collect<Memo[], unknown>(() =>
        repository.search(criteria),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });
  });

  describe("persist/terminate", () => {
    it("persist は読み取り専用エラー", async () => {
      const repository = createFirestoreRestMemoRepository(createStubClient());

      const outcome = await collect<void, unknown>(() =>
        repository.persist({} as never),
      );

      expect(outcome.ok).toBe(false);
      if (!outcome.ok) {
        expect(isUnexpectedError(outcome.error)).toBe(true);
      }
    });

    it("terminate は読み取り専用エラー", async () => {
      const repository = createFirestoreRestMemoRepository(createStubClient());
      const identifier = validateMemoIdentifier(
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
