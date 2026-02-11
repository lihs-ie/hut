import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import {
  unexpectedError,
} from "@shared/aspects/error";
import { PageViewMold } from "../../support/molds/domains/analytics/page-view";
import { ArticleMold } from "../../support/molds/domains/article/common";
import { Command } from "@shared/workflows/common";
import {
  validatePeriod,
} from "@shared/domains/analytics/common";
import { PageView } from "@shared/domains/analytics/page-view";
import {
  SearchReferenceIdentifierMold,
} from "../../support/molds/domains/search-token/common";
import { ContentType } from "@shared/domains/search-token/reference";
import { ArticleIdentifierMold } from "../../support/molds/domains/article/common";
import {
  createGetContentRankingWorkflow,
  createGetTagPageViewsWorkflow,
  createGetContentTypeComparisonWorkflow,
} from "@shared/workflows/analytics/content";

describe("workflows/analytics/content", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createGetContentRankingWorkflow", () => {
    it("有効なperiodで複数コンテンツのPVをvalue降順のRankedItem[]で返す", async () => {
      const contentIdentifierA = Forger(ArticleIdentifierMold).forgeWithSeed(1);
      const contentIdentifierB = Forger(ArticleIdentifierMold).forgeWithSeed(2);

      const referenceA = Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        content: contentIdentifierA,
      });
      const referenceB = Forger(SearchReferenceIdentifierMold).forgeWithSeed(2, {
        type: ContentType.ARTICLE,
        content: contentIdentifierB,
      });

      const basePageViewA = Forger(PageViewMold).forgeWithSeed(1);
      const basePageViewB = Forger(PageViewMold).forgeWithSeed(2);

      const pageViewA1: PageView = {
        ...basePageViewA,
        identifier: { ...basePageViewA.identifier, reference: referenceA },
      };
      const pageViewA2: PageView = {
        ...basePageViewA,
        identifier: { ...basePageViewA.identifier, reference: referenceA },
      };
      const pageViewA3: PageView = {
        ...basePageViewA,
        identifier: { ...basePageViewA.identifier, reference: referenceA },
      };
      const pageViewB1: PageView = {
        ...basePageViewB,
        identifier: { ...basePageViewB.identifier, reference: referenceB },
      };

      const searchMock = vi
        .fn()
        .mockReturnValue(
          ok([pageViewA1, pageViewA2, pageViewA3, pageViewB1]).toAsync()
        );

      const searchArticlesMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const searchMemosMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const workflow = createGetContentRankingWorkflow(validatePeriod)(
        searchMock
      )(searchArticlesMock)(searchMemosMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(2);
      expect(result[0].value).toBe(3);
      expect(result[0].label).toBe(contentIdentifierA);
      expect(result[1].value).toBe(1);
      expect(result[1].label).toBe(contentIdentifierB);
      for (let index = 0; index < result.length - 1; index++) {
        expect(result[index].value).toBeGreaterThanOrEqual(
          result[index + 1].value
        );
      }
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const searchArticlesMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const searchMemosMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const workflow = createGetContentRankingWorkflow(validatePeriod)(
        searchMock
      )(searchArticlesMock)(searchMemosMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "invalid" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(searchMock).not.toHaveBeenCalled();
    });

    it("検索エラー時にUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Search failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const searchArticlesMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const searchMemosMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const workflow = createGetContentRankingWorkflow(validatePeriod)(
        searchMock
      )(searchArticlesMock)(searchMemosMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "30d" },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetTagPageViewsWorkflow", () => {
    it("PageViewとArticleのタグからタグ別PVのRankedItem[]を返す", async () => {
      const article1 = Forger(ArticleMold).forgeWithSeed(1);
      const article2 = Forger(ArticleMold).forgeWithSeed(2);

      const reference1 = Forger(SearchReferenceIdentifierMold).forgeWithSeed(
        1,
        {
          type: ContentType.ARTICLE,
          content: article1.identifier,
        }
      );
      const reference2 = Forger(SearchReferenceIdentifierMold).forgeWithSeed(
        2,
        {
          type: ContentType.ARTICLE,
          content: article2.identifier,
        }
      );

      const basePageView1 = Forger(PageViewMold).forgeWithSeed(1);
      const basePageView2 = Forger(PageViewMold).forgeWithSeed(2);

      const pageView1: PageView = {
        ...basePageView1,
        identifier: { ...basePageView1.identifier, reference: reference1 },
      };
      const pageView2: PageView = {
        ...basePageView1,
        identifier: { ...basePageView1.identifier, reference: reference1 },
      };
      const pageView3: PageView = {
        ...basePageView2,
        identifier: { ...basePageView2.identifier, reference: reference2 },
      };

      const searchPageViewsMock = vi
        .fn()
        .mockReturnValue(ok([pageView1, pageView2, pageView3]).toAsync());
      const searchArticlesMock = vi
        .fn()
        .mockReturnValue(ok([article1, article2]).toAsync());

      const workflow = createGetTagPageViewsWorkflow(validatePeriod)(
        searchPageViewsMock
      )(searchArticlesMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);
      for (let index = 0; index < result.length - 1; index++) {
        expect(result[index].value).toBeGreaterThanOrEqual(
          result[index + 1].value
        );
      }
    });

    it("タグのないArticleに紐づくPVはタグ集計に含まれない", async () => {
      const articleWithoutTags = Forger(ArticleMold).forgeWithSeed(1, {
        tags: [],
      });

      const reference = Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        content: articleWithoutTags.identifier,
      });

      const basePageView = Forger(PageViewMold).forgeWithSeed(1);
      const pageView: PageView = {
        ...basePageView,
        identifier: { ...basePageView.identifier, reference },
      };

      const searchPageViewsMock = vi
        .fn()
        .mockReturnValue(ok([pageView]).toAsync());
      const searchArticlesMock = vi
        .fn()
        .mockReturnValue(ok([articleWithoutTags]).toAsync());

      const workflow = createGetTagPageViewsWorkflow(validatePeriod)(
        searchPageViewsMock
      )(searchArticlesMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(result.length).toBe(0);
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchPageViewsMock = vi.fn();
      const searchArticlesMock = vi.fn();

      const workflow = createGetTagPageViewsWorkflow(validatePeriod)(
        searchPageViewsMock
      )(searchArticlesMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "bad" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(searchPageViewsMock).not.toHaveBeenCalled();
      expect(searchArticlesMock).not.toHaveBeenCalled();
    });

    it("PageView検索エラー時にUnexpectedErrorを返す", async () => {
      const error = unexpectedError("PV search failed");
      const searchPageViewsMock = vi
        .fn()
        .mockReturnValue(err(error).toAsync());
      const searchArticlesMock = vi
        .fn()
        .mockReturnValue(ok([]).toAsync());

      const workflow = createGetTagPageViewsWorkflow(validatePeriod)(
        searchPageViewsMock
      )(searchArticlesMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "30d" },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetContentTypeComparisonWorkflow", () => {
    it("有効なperiodでコンテンツタイプ別のDistribution[]を返す", async () => {
      const articleRef = Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
      });
      const memoRef = Forger(SearchReferenceIdentifierMold).forgeWithSeed(2, {
        type: ContentType.MEMO,
      });

      const basePageView1 = Forger(PageViewMold).forgeWithSeed(1);
      const basePageView2 = Forger(PageViewMold).forgeWithSeed(2);
      const basePageView3 = Forger(PageViewMold).forgeWithSeed(3);

      const articlePageView1: PageView = {
        ...basePageView1,
        identifier: { ...basePageView1.identifier, reference: articleRef },
      };
      const articlePageView2: PageView = {
        ...basePageView2,
        identifier: { ...basePageView2.identifier, reference: articleRef },
      };
      const memoPageView1: PageView = {
        ...basePageView3,
        identifier: { ...basePageView3.identifier, reference: memoRef },
      };

      const searchMock = vi
        .fn()
        .mockReturnValue(
          ok([articlePageView1, articlePageView2, memoPageView1]).toAsync()
        );

      const workflow = createGetContentTypeComparisonWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(2);
      const articleDistribution = result.find(
        (distribution) => distribution.label === "article"
      );
      const memoDistribution = result.find(
        (distribution) => distribution.label === "memo"
      );
      expect(articleDistribution?.value).toBe(2);
      expect(memoDistribution?.value).toBe(1);
      for (let index = 0; index < result.length - 1; index++) {
        expect(result[index].value).toBeGreaterThanOrEqual(
          result[index + 1].value
        );
      }
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetContentTypeComparisonWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "xyz" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(searchMock).not.toHaveBeenCalled();
    });

    it("検索エラー時にUnexpectedErrorを返す", async () => {
      const error = unexpectedError("DB connection failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createGetContentTypeComparisonWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "90d" },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });
});
