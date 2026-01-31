import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import {
  createGetPrivacyPolicyWorkflow,
  createPrivacyPolicyPersistWorkflow,
} from "@shared/workflows/document";
import {
  SiteDocument,
  PrivacyPolicy,
  PrivacyPolicySection,
} from "@shared/domains/document";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  unexpectedError,
} from "@shared/aspects/error";
import { Command } from "@shared/workflows/common";
import { TimelineMold } from "../support/molds/domains/common/date";
import { Timeline } from "@shared/domains/common";

// Mold definitions for document testing
type PrivacyPolicySectionProperties = {
  headline: string;
  body: string;
  list: string[] | null;
};

const PrivacyPolicySectionMold = Mold<
  PrivacyPolicySection,
  PrivacyPolicySectionProperties
>({
  pour: (properties) =>
    ({
      headline: properties.headline,
      body: properties.body,
      list: properties.list,
    }) as PrivacyPolicySection,
  prepare: (overrides, seed) => ({
    headline:
      overrides.headline ?? Forger(StringMold(1, 100)).forgeWithSeed(seed),
    body: overrides.body ?? Forger(StringMold(1, 500)).forgeWithSeed(seed + 1),
    list: overrides.list ?? null,
  }),
});

type PrivacyPolicyProperties = {
  sections: PrivacyPolicySection[];
  timeline: Timeline;
};

const PrivacyPolicyMold = Mold<PrivacyPolicy, PrivacyPolicyProperties>({
  pour: (properties) =>
    ({
      sections: properties.sections,
      timeline: properties.timeline,
    }) as PrivacyPolicy,
  prepare: (overrides, seed) => ({
    sections:
      overrides.sections ??
      Forger(PrivacyPolicySectionMold).forgeMultiWithSeed(3, seed),
    timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
  }),
});

type SiteDocumentProperties = {
  privacy: PrivacyPolicy;
};

const SiteDocumentMold = Mold<SiteDocument, SiteDocumentProperties>({
  pour: (properties) =>
    ({
      privacy: properties.privacy,
    }) as SiteDocument,
  prepare: (overrides, seed) => ({
    privacy: overrides.privacy ?? Forger(PrivacyPolicyMold).forgeWithSeed(seed),
  }),
});

describe("workflows/document", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createGetPrivacyPolicyWorkflow", () => {
    it("プライバシーポリシーを取得できる", async () => {
      const siteDocument = Forger(SiteDocumentMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(siteDocument).toAsync());

      const workflow = createGetPrivacyPolicyWorkflow(findMock)(mockLogger);

      const command: Command<null> = {
        now: new Date(),
        payload: null,
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(siteDocument.privacy);
      expect(findMock).toHaveBeenCalled();
    });

    it("ドキュメントが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const notFoundError = aggregateNotFoundError(
        "SiteDocument",
        "SiteDocument not found"
      );
      const findMock = vi.fn().mockReturnValue(err(notFoundError).toAsync());

      const workflow = createGetPrivacyPolicyWorkflow(findMock)(mockLogger);

      const command: Command<null> = {
        now: new Date(),
        payload: null,
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });

    it("予期しないエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Database connection failed");
      const findMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createGetPrivacyPolicyWorkflow(findMock)(mockLogger);

      const command: Command<null> = {
        now: new Date(),
        payload: null,
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createPrivacyPolicyPersistWorkflow", () => {
    it("プライバシーポリシーを更新できる", async () => {
      const siteDocument = Forger(SiteDocumentMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(siteDocument).toAsync());
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createPrivacyPolicyPersistWorkflow(findMock)(persistMock)(
        mockLogger
      );

      const newSections = [
        {
          headline: "New Privacy Policy",
          body: "This is the new privacy policy body.",
          list: null,
        },
      ];

      const command: Command<{
        sections: Array<{
          headline: string;
          body: string;
          list: string[] | null;
        }>;
      }> = {
        now: new Date(),
        payload: {
          sections: newSections,
        },
      };

      const result = await workflow(command).unwrap();

      expect(result).toBeUndefined();
      expect(findMock).toHaveBeenCalled();
      expect(persistMock).toHaveBeenCalled();
    });

    it("ドキュメントが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const notFoundError = aggregateNotFoundError(
        "SiteDocument",
        "SiteDocument not found"
      );
      const findMock = vi.fn().mockReturnValue(err(notFoundError).toAsync());
      const persistMock = vi.fn();

      const workflow = createPrivacyPolicyPersistWorkflow(findMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        sections: Array<{
          headline: string;
          body: string;
          list: string[] | null;
        }>;
      }> = {
        now: new Date(),
        payload: {
          sections: [
            {
              headline: "Test",
              body: "Test body",
              list: null,
            },
          ],
        },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("無効なセクションデータでValidationErrorを返す", async () => {
      const siteDocument = Forger(SiteDocumentMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(siteDocument).toAsync());
      const persistMock = vi.fn();

      const workflow = createPrivacyPolicyPersistWorkflow(findMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        sections: Array<{
          headline: string;
          body: string;
          list: string[] | null;
        }>;
      }> = {
        now: new Date(),
        payload: {
          sections: [
            {
              headline: "", // 空のヘッドラインは無効
              body: "",
              list: null,
            },
          ],
        },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化でエラーが発生した場合はエラーを返す", async () => {
      const siteDocument = Forger(SiteDocumentMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(siteDocument).toAsync());
      const error = unexpectedError("Persist failed");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createPrivacyPolicyPersistWorkflow(findMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        sections: Array<{
          headline: string;
          body: string;
          list: string[] | null;
        }>;
      }> = {
        now: new Date(),
        payload: {
          sections: [
            {
              headline: "Valid Headline",
              body: "Valid body content",
              list: null,
            },
          ],
        },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });

    it("リスト付きのセクションを更新できる", async () => {
      const siteDocument = Forger(SiteDocumentMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(siteDocument).toAsync());
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createPrivacyPolicyPersistWorkflow(findMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        sections: Array<{
          headline: string;
          body: string;
          list: string[] | null;
        }>;
      }> = {
        now: new Date(),
        payload: {
          sections: [
            {
              headline: "Section with List",
              body: "This section has a list of items.",
              list: ["Item 1", "Item 2", "Item 3"],
            },
          ],
        },
      };

      const result = await workflow(command).unwrap();

      expect(result).toBeUndefined();
      expect(persistMock).toHaveBeenCalled();
    });
  });
});
