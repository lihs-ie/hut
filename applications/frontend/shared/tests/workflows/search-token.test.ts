import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { createSearchByTokenWorkflow } from "@shared/workflows/search-token";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import { unexpectedError } from "@shared/aspects/error";
import {
  validateCriteria,
  SearchTokenType,
  ContentType,
  searchTokenValueSchema,
  createSearchTokenIdentifier,
} from "@shared/domains/search-token";
import {
  SearchTokenMold,
  SearchReferenceMold,
  SearchReferenceIdentifierMold,
} from "../support/molds/domains/search-token";
import { ArticleMold } from "../support/molds/domains/article";
import { MemoMold } from "../support/molds/domains/memo";
import { TagIdentifierMold } from "../support/molds/domains/attributes/tag";

describe("workflows/search-token", () => {
  const logger = Logger(Environment.DEVELOPMENT);

  const createCommand = (payload: {
    freeWord: string | null;
    tags: string[] | null;
    type: ContentType | string | null;
    sortBy: string | null;
    order: string | null;
    limit: number | null;
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
    limit: null,
  });

  const emptyAsyncResult = () => ok([]).toAsync();

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createSearchByTokenWorkflow", () => {
    const createWorkflow = (
      ofTokenIdentifiersMock: ReturnType<typeof vi.fn>,
      ofArticleIdentifiersMock: ReturnType<typeof vi.fn>,
      ofMemoIdentifiersMock: ReturnType<typeof vi.fn>,
      ofSeriesIdentifiersMock: ReturnType<typeof vi.fn>
    ) =>
      createSearchByTokenWorkflow(validateCriteria)(logger)(
        ofTokenIdentifiersMock
      )(ofArticleIdentifiersMock)(ofMemoIdentifiersMock)(
        ofSeriesIdentifiersMock
      );

    const createNgramToken = (
      article: ReturnType<typeof Forger<typeof ArticleMold>["forgeWithSeed"]>
    ) => {
      const referenceIdentifier = Forger(
        SearchReferenceIdentifierMold
      ).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        content: article.identifier,
      });
      const reference = Forger(SearchReferenceMold).forgeWithSeed(1, {
        identifier: referenceIdentifier,
      });
      const tokenValue = searchTokenValueSchema.parse("te");
      const tokenIdentifier = createSearchTokenIdentifier(
        SearchTokenType.NGRAM,
        tokenValue
      );
      return Forger(SearchTokenMold).forgeWithSeed(1, {
        identifier: tokenIdentifier,
        type: SearchTokenType.NGRAM,
        value: tokenValue,
        references: [reference],
      });
    };

    it("空の検索条件で空の結果を返す", async () => {
      const ofTokenIdentifiersMock = vi
        .fn()
        .mockReturnValue(emptyAsyncResult());
      const workflow = createWorkflow(
        ofTokenIdentifiersMock,
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult())
      );

      const result = await workflow(
        createCommand(createEmptySearchPayload())
      ).unwrap();

      expect(result).toEqual([]);
      expect(ofTokenIdentifiersMock).not.toHaveBeenCalled();
    });

    it("freeWordを指定して検索できる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const token = createNgramToken(article);

      const ofTokenIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok([token]).toAsync());
      const ofArticleIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok([article]).toAsync());
      const workflow = createWorkflow(
        ofTokenIdentifiersMock,
        ofArticleIdentifiersMock,
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult())
      );

      const result = await workflow(
        createCommand({ ...createEmptySearchPayload(), freeWord: "test" })
      ).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(ofTokenIdentifiersMock).toHaveBeenCalled();
    });

    it("tagsを指定して検索できる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const tagIdentifier = Forger(TagIdentifierMold).forgeWithSeed(1);
      const referenceIdentifier = Forger(
        SearchReferenceIdentifierMold
      ).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        content: article.identifier,
      });
      const reference = Forger(SearchReferenceMold).forgeWithSeed(1, {
        identifier: referenceIdentifier,
      });
      const tokenValue = searchTokenValueSchema.parse(tagIdentifier);
      const tokenIdentifier = createSearchTokenIdentifier(
        SearchTokenType.TAG,
        tokenValue
      );
      const token = Forger(SearchTokenMold).forgeWithSeed(1, {
        identifier: tokenIdentifier,
        type: SearchTokenType.TAG,
        value: tokenValue,
        references: [reference],
      });

      const ofTokenIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok([token]).toAsync());
      const ofArticleIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok([article]).toAsync());
      const workflow = createWorkflow(
        ofTokenIdentifiersMock,
        ofArticleIdentifiersMock,
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult())
      );

      const result = await workflow(
        createCommand({
          ...createEmptySearchPayload(),
          tags: [tagIdentifier],
        })
      ).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(ofTokenIdentifiersMock).toHaveBeenCalled();
    });

    it("無効なバリデーションでValidationErrorを返す", async () => {
      const ofTokenIdentifiersMock = vi.fn();
      const workflow = createWorkflow(
        ofTokenIdentifiersMock,
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
          limit: null,
        })
      ).match({
        ok: () => false,
        err: () => true,
      });

      expect(result).toBe(true);
      expect(ofTokenIdentifiersMock).not.toHaveBeenCalled();
    });

    it("トークン取得でエラーが発生した場合はエラーを返す", async () => {
      const error = unexpectedError("Token retrieval failed");
      const ofTokenIdentifiersMock = vi
        .fn()
        .mockReturnValue(err(error).toAsync());
      const workflow = createWorkflow(
        ofTokenIdentifiersMock,
        vi.fn(),
        vi.fn(),
        vi.fn()
      );

      const result = await workflow(
        createCommand({ ...createEmptySearchPayload(), freeWord: "test" })
      ).unwrapError();

      expect(result).toEqual(error);
    });

    it("記事取得でエラーが発生した場合はエラーを返す", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const token = createNgramToken(article);
      const error = unexpectedError("Article retrieval failed");

      const ofTokenIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok([token]).toAsync());
      const ofArticleIdentifiersMock = vi
        .fn()
        .mockReturnValue(err(error).toAsync());
      const workflow = createWorkflow(
        ofTokenIdentifiersMock,
        ofArticleIdentifiersMock,
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult())
      );

      const result = await workflow(
        createCommand({ ...createEmptySearchPayload(), freeWord: "test" })
      ).unwrapError();

      expect(result).toEqual(error);
    });

    it("typeでフィルタリングできる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const memo = Forger(MemoMold).forgeWithSeed(2);

      const articleReferenceIdentifier = Forger(
        SearchReferenceIdentifierMold
      ).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        content: article.identifier,
      });
      const articleReference = Forger(SearchReferenceMold).forgeWithSeed(1, {
        identifier: articleReferenceIdentifier,
      });

      const memoReferenceIdentifier = Forger(
        SearchReferenceIdentifierMold
      ).forgeWithSeed(2, {
        type: ContentType.MEMO,
        content: memo.identifier,
      });
      const memoReference = Forger(SearchReferenceMold).forgeWithSeed(2, {
        identifier: memoReferenceIdentifier,
      });

      const tokenValue = searchTokenValueSchema.parse("te");
      const tokenIdentifier = createSearchTokenIdentifier(
        SearchTokenType.NGRAM,
        tokenValue
      );
      const token = Forger(SearchTokenMold).forgeWithSeed(1, {
        identifier: tokenIdentifier,
        type: SearchTokenType.NGRAM,
        value: tokenValue,
        references: [articleReference, memoReference],
      });

      const ofTokenIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok([token]).toAsync());
      const ofArticleIdentifiersMock = vi
        .fn()
        .mockReturnValue(ok([article]).toAsync());
      const workflow = createWorkflow(
        ofTokenIdentifiersMock,
        ofArticleIdentifiersMock,
        vi.fn().mockReturnValue(emptyAsyncResult()),
        vi.fn().mockReturnValue(emptyAsyncResult())
      );

      const result = await workflow(
        createCommand({
          ...createEmptySearchPayload(),
          freeWord: "test",
          type: ContentType.ARTICLE,
        })
      ).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(ofArticleIdentifiersMock).toHaveBeenCalled();
    });
  });
});
