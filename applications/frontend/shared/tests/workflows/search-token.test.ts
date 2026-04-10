import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { createSearchByTokenWorkflow } from "@shared/workflows/search-token";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import { unexpectedError } from "@shared/aspects/error";
import {
  SearchTokenRepository,
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
import { TimelineMold } from "../support/molds/domains/common/date";
import type { Article, ArticleRepository } from "@shared/domains/articles";
import type { MemoRepository } from "@shared/domains/memo";
import type { SeriesRepository } from "@shared/domains/series";

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
      ofTokenIdentifiersMock: SearchTokenRepository["ofIdentifiers"],
      ofArticleIdentifiersMock: ArticleRepository["ofIdentifiers"],
      ofMemoIdentifiersMock: MemoRepository["ofIdentifiers"],
      ofSeriesIdentifiersMock: SeriesRepository["ofIdentifiers"]
    ) =>
      createSearchByTokenWorkflow(validateCriteria)(logger)(
        ofTokenIdentifiersMock
      )(ofArticleIdentifiersMock)(ofMemoIdentifiersMock)(
        ofSeriesIdentifiersMock
      );

    const createNgramToken = (
      article: Article
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

    describe("ソート", () => {
      const createArticleWithTimeline = (
        seed: number,
        createdAt: Date,
        updatedAt: Date
      ): Article => {
        const timeline = Forger(TimelineMold).forgeWithSeed(seed, {
          createdAt,
          updatedAt,
        });
        return Forger(ArticleMold).forgeWithSeed(seed, { timeline });
      };

      const createNgramTokenForArticles = (articles: Article[]) => {
        const references = articles.map((article, index) => {
          const referenceIdentifier = Forger(
            SearchReferenceIdentifierMold
          ).forgeWithSeed(index + 10, {
            type: ContentType.ARTICLE,
            content: article.identifier,
          });
          return Forger(SearchReferenceMold).forgeWithSeed(index + 10, {
            identifier: referenceIdentifier,
          });
        });
        const tokenValue = searchTokenValueSchema.parse("te");
        const tokenIdentifier = createSearchTokenIdentifier(
          SearchTokenType.NGRAM,
          tokenValue
        );
        return Forger(SearchTokenMold).forgeWithSeed(10, {
          identifier: tokenIdentifier,
          type: SearchTokenType.NGRAM,
          value: tokenValue,
          references,
        });
      };

      it("LATEST + DESCで更新日時の降順にソートされる", async () => {
        const older = createArticleWithTimeline(
          10,
          new Date("2024-01-01"),
          new Date("2024-01-10")
        );
        const newer = createArticleWithTimeline(
          11,
          new Date("2024-02-01"),
          new Date("2024-02-10")
        );
        const token = createNgramTokenForArticles([older, newer]);

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token]).toAsync()),
          vi.fn().mockReturnValue(ok([older, newer]).toAsync()),
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
            sortBy: "latest",
            order: "desc",
          })
        ).unwrap();

        expect(result.length).toBe(2);
        expect(result[0].identifier).toEqual(newer.identifier);
        expect(result[1].identifier).toEqual(older.identifier);
      });

      it("LATEST + ASCで更新日時の昇順にソートされる", async () => {
        const older = createArticleWithTimeline(
          10,
          new Date("2024-01-01"),
          new Date("2024-01-10")
        );
        const newer = createArticleWithTimeline(
          11,
          new Date("2024-02-01"),
          new Date("2024-02-10")
        );
        const token = createNgramTokenForArticles([older, newer]);

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token]).toAsync()),
          vi.fn().mockReturnValue(ok([older, newer]).toAsync()),
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
            sortBy: "latest",
            order: "asc",
          })
        ).unwrap();

        expect(result.length).toBe(2);
        expect(result[0].identifier).toEqual(older.identifier);
        expect(result[1].identifier).toEqual(newer.identifier);
      });

      it("NEWEST + DESCで作成日時の降順にソートされる", async () => {
        const older = createArticleWithTimeline(
          10,
          new Date("2024-01-01"),
          new Date("2024-01-10")
        );
        const newer = createArticleWithTimeline(
          11,
          new Date("2024-02-01"),
          new Date("2024-02-10")
        );
        const token = createNgramTokenForArticles([older, newer]);

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token]).toAsync()),
          vi.fn().mockReturnValue(ok([older, newer]).toAsync()),
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
            sortBy: "newest",
            order: "desc",
          })
        ).unwrap();

        expect(result.length).toBe(2);
        expect(result[0].identifier).toEqual(newer.identifier);
        expect(result[1].identifier).toEqual(older.identifier);
      });

      it("NEWEST + ASCで作成日時の昇順にソートされる", async () => {
        const older = createArticleWithTimeline(
          10,
          new Date("2024-01-01"),
          new Date("2024-01-10")
        );
        const newer = createArticleWithTimeline(
          11,
          new Date("2024-02-01"),
          new Date("2024-02-10")
        );
        const token = createNgramTokenForArticles([older, newer]);

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token]).toAsync()),
          vi.fn().mockReturnValue(ok([older, newer]).toAsync()),
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
            sortBy: "newest",
            order: "asc",
          })
        ).unwrap();

        expect(result.length).toBe(2);
        expect(result[0].identifier).toEqual(older.identifier);
        expect(result[1].identifier).toEqual(newer.identifier);
      });

      it("sortByがnullの場合はソートしない", async () => {
        const article1 = createArticleWithTimeline(
          10,
          new Date("2024-01-01"),
          new Date("2024-01-10")
        );
        const article2 = createArticleWithTimeline(
          11,
          new Date("2024-02-01"),
          new Date("2024-02-10")
        );
        const token = createNgramTokenForArticles([article1, article2]);

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token]).toAsync()),
          vi.fn().mockReturnValue(ok([article1, article2]).toAsync()),
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
            sortBy: null,
            order: "desc",
          })
        ).unwrap();

        expect(result.length).toBe(2);
        expect(result[0].identifier).toEqual(article1.identifier);
        expect(result[1].identifier).toEqual(article2.identifier);
      });
    });

    describe("複合検索", () => {
      it("freeWordとtagsの両方を指定すると積集合で検索される", async () => {
        const articleInBoth = Forger(ArticleMold).forgeWithSeed(1);
        const articleNgramOnly = Forger(ArticleMold).forgeWithSeed(2);
        const tagIdentifier = Forger(TagIdentifierMold).forgeWithSeed(1);

        const ngramRefForBoth = Forger(SearchReferenceMold).forgeWithSeed(1, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: articleInBoth.identifier,
          }),
        });
        const ngramRefNgramOnly = Forger(SearchReferenceMold).forgeWithSeed(2, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(2, {
            type: ContentType.ARTICLE,
            content: articleNgramOnly.identifier,
          }),
        });
        const tagRefForBoth = Forger(SearchReferenceMold).forgeWithSeed(3, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(3, {
            type: ContentType.ARTICLE,
            content: articleInBoth.identifier,
          }),
        });

        const ngramTokenValue = searchTokenValueSchema.parse("te");
        const ngramTokenIdentifier = createSearchTokenIdentifier(
          SearchTokenType.NGRAM,
          ngramTokenValue
        );
        const ngramToken = Forger(SearchTokenMold).forgeWithSeed(1, {
          identifier: ngramTokenIdentifier,
          type: SearchTokenType.NGRAM,
          value: ngramTokenValue,
          references: [ngramRefForBoth, ngramRefNgramOnly],
        });

        const tagTokenValue = searchTokenValueSchema.parse(tagIdentifier);
        const tagTokenIdentifier = createSearchTokenIdentifier(
          SearchTokenType.TAG,
          tagTokenValue
        );
        const tagToken = Forger(SearchTokenMold).forgeWithSeed(2, {
          identifier: tagTokenIdentifier,
          type: SearchTokenType.TAG,
          value: tagTokenValue,
          references: [tagRefForBoth],
        });

        const ofArticleIdentifiersMock = vi
          .fn()
          .mockReturnValue(ok([articleInBoth]).toAsync());

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([ngramToken, tagToken]).toAsync()),
          ofArticleIdentifiersMock,
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
            tags: [tagIdentifier],
          })
        ).unwrap();

        expect(Array.isArray(result)).toBe(true);
        const identifiers = result.map((item) => item.identifier);
        expect(identifiers).toContainEqual(articleInBoth.identifier);
        expect(identifiers).not.toContainEqual(articleNgramOnly.identifier);
      });

      it("ngramのみの場合はngramの結果を返す", async () => {
        const article = Forger(ArticleMold).forgeWithSeed(1);

        const ngramRef = Forger(SearchReferenceMold).forgeWithSeed(1, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: article.identifier,
          }),
        });

        const ngramTokenValue = searchTokenValueSchema.parse("te");
        const ngramTokenIdentifier = createSearchTokenIdentifier(
          SearchTokenType.NGRAM,
          ngramTokenValue
        );
        const ngramToken = Forger(SearchTokenMold).forgeWithSeed(1, {
          identifier: ngramTokenIdentifier,
          type: SearchTokenType.NGRAM,
          value: ngramTokenValue,
          references: [ngramRef],
        });

        const ofArticleIdentifiersMock = vi
          .fn()
          .mockReturnValue(ok([article]).toAsync());

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([ngramToken]).toAsync()),
          ofArticleIdentifiersMock,
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
          })
        ).unwrap();

        expect(Array.isArray(result)).toBe(true);
        expect(ofArticleIdentifiersMock).toHaveBeenCalled();
      });

      it("tagsのみの場合はtagsの結果を返す", async () => {
        const article = Forger(ArticleMold).forgeWithSeed(1);
        const tagIdentifier = Forger(TagIdentifierMold).forgeWithSeed(1);

        const tagRef = Forger(SearchReferenceMold).forgeWithSeed(1, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: article.identifier,
          }),
        });

        const tagTokenValue = searchTokenValueSchema.parse(tagIdentifier);
        const tagTokenIdentifier = createSearchTokenIdentifier(
          SearchTokenType.TAG,
          tagTokenValue
        );
        const tagToken = Forger(SearchTokenMold).forgeWithSeed(1, {
          identifier: tagTokenIdentifier,
          type: SearchTokenType.TAG,
          value: tagTokenValue,
          references: [tagRef],
        });

        const ofArticleIdentifiersMock = vi
          .fn()
          .mockReturnValue(ok([article]).toAsync());

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([tagToken]).toAsync()),
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
        expect(ofArticleIdentifiersMock).toHaveBeenCalled();
      });
    });

    describe("typeフィルタ", () => {
      it("type指定ありの場合は指定タイプのみ返す", async () => {
        const article = Forger(ArticleMold).forgeWithSeed(1);
        const memo = Forger(MemoMold).forgeWithSeed(2);

        const articleRef = Forger(SearchReferenceMold).forgeWithSeed(1, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: article.identifier,
          }),
        });
        const memoRef = Forger(SearchReferenceMold).forgeWithSeed(2, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(2, {
            type: ContentType.MEMO,
            content: memo.identifier,
          }),
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
          references: [articleRef, memoRef],
        });

        const ofArticleIdentifiersMock = vi
          .fn()
          .mockImplementation((identifiers: unknown[]) =>
            ok(identifiers.length > 0 ? [article] : []).toAsync()
          );
        const ofMemoIdentifiersMock = vi
          .fn()
          .mockImplementation((identifiers: unknown[]) =>
            ok(identifiers.length > 0 ? [memo] : []).toAsync()
          );

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token]).toAsync()),
          ofArticleIdentifiersMock,
          ofMemoIdentifiersMock,
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
        expect(result.length).toBe(1);
        expect(result[0].identifier).toEqual(article.identifier);
      });

      it("type指定なしの場合はすべてのタイプを返す", async () => {
        const article = Forger(ArticleMold).forgeWithSeed(1);
        const memo = Forger(MemoMold).forgeWithSeed(2);

        const articleRef = Forger(SearchReferenceMold).forgeWithSeed(1, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: article.identifier,
          }),
        });
        const memoRef = Forger(SearchReferenceMold).forgeWithSeed(2, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(2, {
            type: ContentType.MEMO,
            content: memo.identifier,
          }),
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
          references: [articleRef, memoRef],
        });

        const ofArticleIdentifiersMock = vi
          .fn()
          .mockReturnValue(ok([article]).toAsync());
        const ofMemoIdentifiersMock = vi
          .fn()
          .mockReturnValue(ok([memo]).toAsync());

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token]).toAsync()),
          ofArticleIdentifiersMock,
          ofMemoIdentifiersMock,
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
            type: null,
          })
        ).unwrap();

        expect(Array.isArray(result)).toBe(true);
        expect(ofArticleIdentifiersMock).toHaveBeenCalled();
        expect(ofMemoIdentifiersMock).toHaveBeenCalled();
      });
    });

    describe("unionReferences", () => {
      it("複数のngramトークンの参照が和集合でマージされる", async () => {
        const article1 = Forger(ArticleMold).forgeWithSeed(1);
        const article2 = Forger(ArticleMold).forgeWithSeed(2);

        const ref1 = Forger(SearchReferenceMold).forgeWithSeed(1, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: article1.identifier,
          }),
        });
        const ref2 = Forger(SearchReferenceMold).forgeWithSeed(2, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(2, {
            type: ContentType.ARTICLE,
            content: article2.identifier,
          }),
        });

        const tokenValue1 = searchTokenValueSchema.parse("te");
        const tokenIdentifier1 = createSearchTokenIdentifier(
          SearchTokenType.NGRAM,
          tokenValue1
        );
        const token1 = Forger(SearchTokenMold).forgeWithSeed(1, {
          identifier: tokenIdentifier1,
          type: SearchTokenType.NGRAM,
          value: tokenValue1,
          references: [ref1],
        });

        const tokenValue2 = searchTokenValueSchema.parse("es");
        const tokenIdentifier2 = createSearchTokenIdentifier(
          SearchTokenType.NGRAM,
          tokenValue2
        );
        const token2 = Forger(SearchTokenMold).forgeWithSeed(2, {
          identifier: tokenIdentifier2,
          type: SearchTokenType.NGRAM,
          value: tokenValue2,
          references: [ref2],
        });

        const ofArticleIdentifiersMock = vi
          .fn()
          .mockReturnValue(ok([article1, article2]).toAsync());

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token1, token2]).toAsync()),
          ofArticleIdentifiersMock,
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
          })
        ).unwrap();

        expect(result.length).toBe(2);
      });

      it("同一コンテンツへの参照が重複している場合はスコアが高い方が採用される", async () => {
        const article = Forger(ArticleMold).forgeWithSeed(1);

        const refLowScore = Forger(SearchReferenceMold).forgeWithSeed(1, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: article.identifier,
          }),
          score: 0.5,
        });
        const refHighScore = Forger(SearchReferenceMold).forgeWithSeed(2, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: article.identifier,
          }),
          score: 0.9,
        });

        const tokenValue1 = searchTokenValueSchema.parse("te");
        const tokenIdentifier1 = createSearchTokenIdentifier(
          SearchTokenType.NGRAM,
          tokenValue1
        );
        const token1 = Forger(SearchTokenMold).forgeWithSeed(1, {
          identifier: tokenIdentifier1,
          type: SearchTokenType.NGRAM,
          value: tokenValue1,
          references: [refLowScore],
        });

        const tokenValue2 = searchTokenValueSchema.parse("es");
        const tokenIdentifier2 = createSearchTokenIdentifier(
          SearchTokenType.NGRAM,
          tokenValue2
        );
        const token2 = Forger(SearchTokenMold).forgeWithSeed(2, {
          identifier: tokenIdentifier2,
          type: SearchTokenType.NGRAM,
          value: tokenValue2,
          references: [refHighScore],
        });

        const ofArticleIdentifiersMock = vi
          .fn()
          .mockReturnValue(ok([article]).toAsync());

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([token1, token2]).toAsync()),
          ofArticleIdentifiersMock,
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            freeWord: "test",
          })
        ).unwrap();

        expect(result.length).toBe(1);
        expect(ofArticleIdentifiersMock).toHaveBeenCalledOnce();
      });
    });

    describe("intersectReferences", () => {
      it("複数のtagトークンの参照が積集合でフィルタされる", async () => {
        const articleInBoth = Forger(ArticleMold).forgeWithSeed(1);
        const articleOnlyInTag1 = Forger(ArticleMold).forgeWithSeed(2);
        const tagIdentifier1 = Forger(TagIdentifierMold).forgeWithSeed(1);
        const tagIdentifier2 = Forger(TagIdentifierMold).forgeWithSeed(2);

        const refBoth1 = Forger(SearchReferenceMold).forgeWithSeed(1, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: articleInBoth.identifier,
          }),
        });
        const refOnlyInTag1 = Forger(SearchReferenceMold).forgeWithSeed(2, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(2, {
            type: ContentType.ARTICLE,
            content: articleOnlyInTag1.identifier,
          }),
        });
        const refBoth2 = Forger(SearchReferenceMold).forgeWithSeed(3, {
          identifier: Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
            type: ContentType.ARTICLE,
            content: articleInBoth.identifier,
          }),
        });

        const tagTokenValue1 = searchTokenValueSchema.parse(tagIdentifier1);
        const tagTokenIdentifier1 = createSearchTokenIdentifier(
          SearchTokenType.TAG,
          tagTokenValue1
        );
        const tagToken1 = Forger(SearchTokenMold).forgeWithSeed(1, {
          identifier: tagTokenIdentifier1,
          type: SearchTokenType.TAG,
          value: tagTokenValue1,
          references: [refBoth1, refOnlyInTag1],
        });

        const tagTokenValue2 = searchTokenValueSchema.parse(tagIdentifier2);
        const tagTokenIdentifier2 = createSearchTokenIdentifier(
          SearchTokenType.TAG,
          tagTokenValue2
        );
        const tagToken2 = Forger(SearchTokenMold).forgeWithSeed(2, {
          identifier: tagTokenIdentifier2,
          type: SearchTokenType.TAG,
          value: tagTokenValue2,
          references: [refBoth2],
        });

        const ofArticleIdentifiersMock = vi
          .fn()
          .mockReturnValue(ok([articleInBoth]).toAsync());

        const workflow = createWorkflow(
          vi.fn().mockReturnValue(ok([tagToken1, tagToken2]).toAsync()),
          ofArticleIdentifiersMock,
          vi.fn().mockReturnValue(emptyAsyncResult()),
          vi.fn().mockReturnValue(emptyAsyncResult())
        );

        const result = await workflow(
          createCommand({
            ...createEmptySearchPayload(),
            tags: [tagIdentifier1, tagIdentifier2],
          })
        ).unwrap();

        expect(Array.isArray(result)).toBe(true);
        const calledWithIdentifiers =
          ofArticleIdentifiersMock.mock.calls[0][0];
        expect(calledWithIdentifiers).toContainEqual(articleInBoth.identifier);
        expect(calledWithIdentifiers).not.toContainEqual(
          articleOnlyInTag1.identifier
        );
      });
    });
  });
});
