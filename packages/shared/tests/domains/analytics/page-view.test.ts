import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  deviceTypeSchema,
  referrerSchema,
  pageViewIdentifierSchema,
  pageViewSchema,
  equalPageViewIdentifier,
  criteriaSchema,
  detectDeviceType,
  extractReferrerDomain,
  validatePageView,
  validateCriteria,
  UnvalidatedPageView,
} from "@shared/domains/analytics/page-view";
import {
  SearchReferenceIdentifierMold,
} from "../../support/molds/domains/search-token/common";
import {
  describeEnumSchema,
  describeObjectSchemaValidation,
} from "../../support/helpers";
import {
  PageViewMold,
  PageViewIdentifierMold,
  PageViewCriteriaMold,
} from "../../support/molds/domains/analytics/page-view";

describe("domains/analytics/page-view", () => {
  // ---------------------------------------------------------------------------
  // Value Objects
  // ---------------------------------------------------------------------------

  describe("deviceTypeSchema", () => {
    describeEnumSchema(
      "DeviceType",
      deviceTypeSchema,
      ["desktop", "mobile", "tablet"],
    );
  });

  describe("referrerSchema", () => {
    describe("有効なReferrerの検証", () => {
      it("rawがnullでも有効", () => {
        const result = referrerSchema.safeParse({ raw: null });
        expect(result.success).toBe(true);
      });

      it("rawが文字列でも有効", () => {
        const result = referrerSchema.safeParse({
          raw: "https://example.com/page",
        });
        expect(result.success).toBe(true);
      });

      it("rawが空文字列でも有効", () => {
        const result = referrerSchema.safeParse({ raw: "" });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なReferrerの検証", () => {
      it("rawが未定義の場合は無効", () => {
        const result = referrerSchema.safeParse({});
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = referrerSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = referrerSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });
    });
  });

  // ---------------------------------------------------------------------------
  // PageViewIdentifier
  // ---------------------------------------------------------------------------

  describe("pageViewIdentifierSchema", () => {
    describeObjectSchemaValidation(
      "PageViewIdentifier",
      pageViewIdentifierSchema,
      () => Forger(PageViewIdentifierMold).forge(),
    );

    it("有効なPageViewIdentifierは検証を通過する", () => {
      const identifier = Forger(PageViewIdentifierMold).forge();
      const result = pageViewIdentifierSchema.safeParse(identifier);
      expect(result.success).toBe(true);
    });

    it("referenceが無効な場合は無効", () => {
      const result = pageViewIdentifierSchema.safeParse({
        reference: { type: "invalid", content: "invalid" },
        dateKey: "2024-01-01",
        sessionKey: crypto.randomUUID(),
      });
      expect(result.success).toBe(false);
    });

    it("dateKeyが不正な形式の場合は無効", () => {
      const reference = Forger(SearchReferenceIdentifierMold).forge();
      const result = pageViewIdentifierSchema.safeParse({
        reference,
        dateKey: "invalid-date",
        sessionKey: crypto.randomUUID(),
      });
      expect(result.success).toBe(false);
    });

    it("sessionKeyが不正な形式の場合は無効", () => {
      const reference = Forger(SearchReferenceIdentifierMold).forge();
      const result = pageViewIdentifierSchema.safeParse({
        reference,
        dateKey: "2024-01-01",
        sessionKey: "not-a-uuid",
      });
      expect(result.success).toBe(false);
    });
  });

  // ---------------------------------------------------------------------------
  // equalPageViewIdentifier
  // ---------------------------------------------------------------------------

  describe("equalPageViewIdentifier", () => {
    it("同一の識別子はtrueを返す", () => {
      const identifier = Forger(PageViewIdentifierMold).forge();
      expect(equalPageViewIdentifier(identifier, identifier)).toBe(true);
    });

    it("全フィールドが一致する場合はtrueを返す", () => {
      const reference = Forger(SearchReferenceIdentifierMold).forge();
      const dateKey = "2024-01-01";
      const sessionKey = crypto.randomUUID();

      const left = pageViewIdentifierSchema.parse({
        reference,
        dateKey,
        sessionKey,
      });
      const right = pageViewIdentifierSchema.parse({
        reference,
        dateKey,
        sessionKey,
      });

      expect(equalPageViewIdentifier(left, right)).toBe(true);
    });

    it("referenceが異なる場合はfalseを返す", () => {
      const reference1 = Forger(SearchReferenceIdentifierMold).forgeWithSeed(100);
      const reference2 = Forger(SearchReferenceIdentifierMold).forgeWithSeed(200);
      const dateKey = "2024-01-01";
      const sessionKey = crypto.randomUUID();

      const identifier1 = pageViewIdentifierSchema.parse({
        reference: reference1,
        dateKey,
        sessionKey,
      });
      const identifier2 = pageViewIdentifierSchema.parse({
        reference: reference2,
        dateKey,
        sessionKey,
      });

      expect(equalPageViewIdentifier(identifier1, identifier2)).toBe(false);
    });

    it("dateKeyが異なる場合はfalseを返す", () => {
      const reference = Forger(SearchReferenceIdentifierMold).forge();
      const sessionKey = crypto.randomUUID();

      const left = pageViewIdentifierSchema.parse({
        reference,
        dateKey: "2024-01-01",
        sessionKey,
      });
      const right = pageViewIdentifierSchema.parse({
        reference,
        dateKey: "2024-01-02",
        sessionKey,
      });

      expect(equalPageViewIdentifier(left, right)).toBe(false);
    });

    it("sessionKeyが異なる場合はfalseを返す", () => {
      const reference = Forger(SearchReferenceIdentifierMold).forge();
      const dateKey = "2024-01-01";

      const left = pageViewIdentifierSchema.parse({
        reference,
        dateKey,
        sessionKey: crypto.randomUUID(),
      });
      const right = pageViewIdentifierSchema.parse({
        reference,
        dateKey,
        sessionKey: crypto.randomUUID(),
      });

      expect(equalPageViewIdentifier(left, right)).toBe(false);
    });
  });

  // ---------------------------------------------------------------------------
  // PageView aggregate
  // ---------------------------------------------------------------------------

  describe("pageViewSchema", () => {
    describeObjectSchemaValidation(
      "PageView",
      pageViewSchema,
      () => Forger(PageViewMold).forge(),
    );

    it("有効なPageViewは検証を通過する", () => {
      const pageView = Forger(PageViewMold).forge();
      const result = pageViewSchema.safeParse(pageView);
      expect(result.success).toBe(true);
    });

    it("identifierが無効な場合は無効", () => {
      const result = pageViewSchema.safeParse({
        identifier: null,
        referrer: { raw: null },
        deviceType: "desktop",
        createdAt: new Date(),
      });
      expect(result.success).toBe(false);
    });

    it("deviceTypeが無効な場合は無効", () => {
      const identifier = Forger(PageViewIdentifierMold).forge();
      const result = pageViewSchema.safeParse({
        identifier,
        referrer: { raw: null },
        deviceType: "invalid",
        createdAt: new Date(),
      });
      expect(result.success).toBe(false);
    });
  });

  // ---------------------------------------------------------------------------
  // validatePageView
  // ---------------------------------------------------------------------------

  describe("validatePageView", () => {
    it("有効なUnvalidatedPageViewでokを返す", () => {
      const reference = Forger(SearchReferenceIdentifierMold).forge();
      const unvalidated: UnvalidatedPageView = {
        identifier: {
          reference: { type: reference.type, content: reference.content },
          dateKey: "2024-01-01",
          sessionKey: crypto.randomUUID(),
        },
        referrer: { raw: "https://example.com" },
        deviceType: "desktop",
        createdAt: new Date(),
      };

      const result = validatePageView(unvalidated);
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedPageViewでerrを返す", () => {
      const unvalidated: UnvalidatedPageView = {
        identifier: {
          reference: { type: "invalid", content: "invalid" },
          dateKey: "invalid",
          sessionKey: "invalid",
        },
        referrer: { raw: null },
        deviceType: "invalid",
        createdAt: new Date(),
      };

      const result = validatePageView(unvalidated);
      expect(result.isErr).toBe(true);
    });
  });

  // ---------------------------------------------------------------------------
  // Criteria
  // ---------------------------------------------------------------------------

  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("全てnullishで有効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: null,
          reference: null,
          deviceType: null,
        });
        expect(result.success).toBe(true);
      });

      it("全てundefinedで有効", () => {
        const result = criteriaSchema.safeParse({});
        expect(result.success).toBe(true);
      });

      it("deviceTypeのみ指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          deviceType: "mobile",
        });
        expect(result.success).toBe(true);
      });

      it("referenceのみ指定しても有効", () => {
        const reference = Forger(SearchReferenceIdentifierMold).forge();
        const result = criteriaSchema.safeParse({
          reference,
        });
        expect(result.success).toBe(true);
      });

      it("Forgerで生成したCriteriaは有効", () => {
        const criteria = Forger(PageViewCriteriaMold).forge();
        const result = criteriaSchema.safeParse(criteria);
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("無効なdeviceTypeの場合は無効", () => {
        const result = criteriaSchema.safeParse({ deviceType: "unknown" });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({
        dateRange: null,
        reference: null,
        deviceType: "desktop",
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なCriteriaでerrを返す", () => {
      const result = validateCriteria({
        dateRange: null,
        reference: null,
        deviceType: "invalid",
      });
      expect(result.isErr).toBe(true);
    });
  });

  // ---------------------------------------------------------------------------
  // Domain Logic: detectDeviceType
  // ---------------------------------------------------------------------------

  describe("detectDeviceType", () => {
    it("nullのUserAgentはdesktopを返す", () => {
      const result = detectDeviceType(null);
      expect(result).toBe("desktop");
    });

    it("空文字列のUserAgentはdesktopを返す", () => {
      const result = detectDeviceType("");
      expect(result).toBe("desktop");
    });

    it("Windowsブラウザのデスクトップ判定", () => {
      const result = detectDeviceType(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      );
      expect(result).toBe("desktop");
    });

    it("iPadはtabletを返す", () => {
      const result = detectDeviceType(
        "Mozilla/5.0 (iPad; CPU OS 14_0 like Mac OS X)",
      );
      expect(result).toBe("tablet");
    });

    it("tabletキーワードを含むUAはtabletを返す", () => {
      const result = detectDeviceType(
        "Mozilla/5.0 (Linux; Android 10; Tablet) AppleWebKit/537.36",
      );
      expect(result).toBe("tablet");
    });

    it("iPhoneはmobileを返す", () => {
      const result = detectDeviceType(
        "Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X)",
      );
      expect(result).toBe("mobile");
    });

    it("Androidモバイルはmobileを返す", () => {
      const result = detectDeviceType(
        "Mozilla/5.0 (Linux; Android 11; Pixel 5) AppleWebKit/537.36",
      );
      expect(result).toBe("mobile");
    });

    it("Mobileキーワードを含むUAはmobileを返す", () => {
      const result = detectDeviceType(
        "Mozilla/5.0 (Linux; Android 12) Mobile Safari/537.36",
      );
      expect(result).toBe("mobile");
    });

    it("戻り値はBrandedなDeviceType型である", () => {
      const result = detectDeviceType("some user agent");
      // DeviceTypeスキーマでの検証が通ることを確認
      const parsed = deviceTypeSchema.safeParse(result);
      expect(parsed.success).toBe(true);
    });
  });

  // ---------------------------------------------------------------------------
  // Domain Logic: extractReferrerDomain
  // ---------------------------------------------------------------------------

  describe("extractReferrerDomain", () => {
    it("rawがnullの場合はDirectを返す", () => {
      const referrer = referrerSchema.parse({ raw: null });
      expect(extractReferrerDomain(referrer)).toBe("Direct");
    });

    it("rawが空文字列の場合はDirectを返す", () => {
      const referrer = referrerSchema.parse({ raw: "" });
      expect(extractReferrerDomain(referrer)).toBe("Direct");
    });

    it("有効なURLからhostnameを抽出する", () => {
      const referrer = referrerSchema.parse({
        raw: "https://www.google.com/search?q=test",
      });
      expect(extractReferrerDomain(referrer)).toBe("www.google.com");
    });

    it("HTTPのURLからhostnameを抽出する", () => {
      const referrer = referrerSchema.parse({
        raw: "http://example.com/page",
      });
      expect(extractReferrerDomain(referrer)).toBe("example.com");
    });

    it("無効なURLの場合はDirectを返す", () => {
      const referrer = referrerSchema.parse({ raw: "not-a-valid-url" });
      expect(extractReferrerDomain(referrer)).toBe("Direct");
    });

    it("サブドメインを含むURLからhostnameを抽出する", () => {
      const referrer = referrerSchema.parse({
        raw: "https://blog.example.com/post/123",
      });
      expect(extractReferrerDomain(referrer)).toBe("blog.example.com");
    });
  });
});
