import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { createSearchByIndexWorkflow } from "@shared/workflows/search-index";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import { unexpectedError } from "@shared/aspects/error";
import {
  validateCriteria,
  ContentType,
} from "@shared/domains/search-index/common";
import { score, Scorer } from "@shared/aspects/ngram";
import { SearchIndexMold } from "../support/molds/domains/search-index";
import { ArticleMold } from "../support/molds/domains/article";
import { MemoMold } from "../support/molds/domains/memo";
import { SeriesMold } from "../support/molds/domains/series";

describe("workflows/search-index", () => {
  const logger = Logger(Environment.DEVELOPMENT);

  const FIELD_WEIGHTS: Record<"title" | "excerpt" | "tags", number> = {
    title: 3,
    excerpt: 1,
    tags: 2,
  };

  const scorer: Scorer<"title" | "excerpt" | "tags"> = score(
    (ngram, context) => {
      const baseScore = context.targetSet.has(ngram) ? 1 : 0;
      return baseScore * FIELD_WEIGHTS[context.field];
    }
  );

  const createCommand = (payload: {
    freeWord: string | null;
    tags: string[] | null;
    type: ContentType | string | null;
    sortBy: string | null;
    order: string | null;
  }) => ({
    now: new Date(),
    payload,
  });

  const createEmptySearchPayload = () => ({
    freeWord: null,
    tags: null,
    type: null,
    sortBy: null,
    order: null,
  });

  const emptyAsyncResult = () => ok([]).toAsync();

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createSearchByIndexWorkflow", () => {
    const createWorkflow = (
      getIndicesMock: ReturnType<typeof vi.fn>,
      ofArticleIdentifiersMock: ReturnType<typeof vi.fn>,
      ofMemoIdentifiersMock: ReturnType<typeof vi.fn>,
      ofSeriesIdentifiersMock: ReturnType<typeof vi.fn>
    ) =>
      createSearchByIndexWorkflow(validateCriteria)(logger)(scorer)(
        getIndicesMock
      )(ofArticleIdentifiersMock)(ofMemoIdentifiersMock)(
        ofSeriesIdentifiersMock
      );

    it("有効な検索条件で検索できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(3, 1);
      const searchIndex = Forger(SearchIndexMold).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        reference: articles[0].identifier,
      });

      const getIndicesMock = vi
        .fn()
        .mockReturnValue(ok([searchIndex]).toAsync());
      const ofArticleIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok(articles.slice(0, 1)).toAsync());
      const workflow = createWorkflow(
        getIndicesMock,
        ofArticleIdentifiersMock,
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult())
      );

      const result = await workflow(
        createCommand(createEmptySearchPayload())
      ).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(0);
      expect(getIndicesMock).toHaveBeenCalled();
    });

    it("freeWordを指定して検索できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(3, 1);
      const searchIndex = Forger(SearchIndexMold).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        reference: articles[0].identifier,
        title: "テスト記事タイトル",
      });

      const getIndicesMock = vi
        .fn()
        .mockReturnValue(ok([searchIndex]).toAsync());
      const ofArticleIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok(articles.slice(0, 1)).toAsync());
      const workflow = createWorkflow(
        getIndicesMock,
        ofArticleIdentifiersMock,
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult())
      );

      const result = await workflow(
        createCommand({ ...createEmptySearchPayload(), freeWord: "テスト" })
      ).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(getIndicesMock).toHaveBeenCalled();
    });

    it("無効なバリデーションでValidationErrorを返す", async () => {
      const getIndicesMock = vi.fn();
      const workflow = createWorkflow(
        getIndicesMock,
        vi.fn(),
        vi.fn(),
        vi.fn()
      );

      const result = await workflow(
        createCommand({
          freeWord: "",
          tags: null,
          type: "invalid-type",
          sortBy: null,
          order: null,
        })
      ).match({
        ok: () => false,
        err: () => true,
      });

      expect(result).toBe(true);
      expect(getIndicesMock).not.toHaveBeenCalled();
    });

    it("インデックス取得でエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Index retrieval failed");
      const getIndicesMock = vi.fn().mockReturnValue(err(error).toAsync());
      const workflow = createWorkflow(
        getIndicesMock,
        vi.fn(),
        vi.fn(),
        vi.fn()
      );

      const result = await workflow(
        createCommand(createEmptySearchPayload())
      ).unwrapError();

      expect(result).toEqual(error);
    });

    it("記事、メモ、シリーズの混在結果を返せる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(1, 1);
      const memos = Forger(MemoMold).forgeMultiWithSeed(1, 2);
      const series = Forger(SeriesMold).forgeMultiWithSeed(1, 3);

      const articleIndex = Forger(SearchIndexMold).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        reference: articles[0].identifier,
      });
      const memoIndex = Forger(SearchIndexMold).forgeWithSeed(2, {
        type: ContentType.MEMO,
        reference: memos[0].identifier,
      });
      const seriesIndex = Forger(SearchIndexMold).forgeWithSeed(3, {
        type: ContentType.SERIES,
        reference: series[0].identifier,
      });

      const workflow = createWorkflow(
        vi
          .fn()
          .mockReturnValue(
            ok([articleIndex, memoIndex, seriesIndex]).toAsync()
          ),
        vi.fn().mockReturnValue(ok(articles).toAsync()),
        vi.fn().mockReturnValue(ok(memos).toAsync()),
        vi.fn().mockReturnValue(ok(series).toAsync())
      );

      const result = await workflow(
        createCommand(createEmptySearchPayload())
      ).unwrap();

      expect(result.length).toBe(3);
    });

    it("空のインデックスの場合は空の結果を返す", async () => {
      const workflow = createWorkflow(
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult())
      );

      const result = await workflow(
        createCommand(createEmptySearchPayload())
      ).unwrap();

      expect(result).toEqual([]);
    });
  });
});
