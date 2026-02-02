import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  privacyPolicySection,
  validatePrivacyPolicySection,
  privacyPolicySchema,
  validatePrivacyPolicy,
  siteDocumentSchema,
  validateSiteDocument,
  updatePrivacy,
} from "@shared/domains/document";
import {
  PrivacyPolicySectionMold,
  PrivacyPolicyMold,
  SiteDocumentMold,
} from "../../support/molds/domains/document";
import { TimelineMold } from "../../support/molds/domains/common/date";

describe("domains/document/common", () => {
  describe("privacyPolicySection", () => {
    describe("有効なPrivacyPolicySectionの検証", () => {
      it("Forgerで生成したPrivacyPolicySectionは有効", () => {
        const section = Forger(PrivacyPolicySectionMold).forge();
        const result = privacyPolicySection.safeParse(section);
        expect(result.success).toBe(true);
      });

      it("listがnullでも有効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "テストヘッドライン",
          body: "テスト本文",
          list: null,
        });
        expect(result.success).toBe(true);
      });

      it("listが配列でも有効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "ヘッドライン",
          body: "本文",
          list: ["項目1", "項目2", "項目3"],
        });
        expect(result.success).toBe(true);
      });

      it("headlineが1文字でも有効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "a",
          body: "本文",
          list: null,
        });
        expect(result.success).toBe(true);
      });

      it("headlineが200文字でも有効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "a".repeat(200),
          body: "本文",
          list: null,
        });
        expect(result.success).toBe(true);
      });

      it("複数のPrivacyPolicySectionを生成できる", () => {
        const sections = Forger(PrivacyPolicySectionMold).forgeMulti(5);
        for (const section of sections) {
          const result = privacyPolicySection.safeParse(section);
          expect(result.success).toBe(true);
        }
      });
    });

    describe("無効なPrivacyPolicySectionの検証", () => {
      it("headlineが空文字列の場合は無効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "",
          body: "本文",
          list: null,
        });
        expect(result.success).toBe(false);
      });

      it("headlineが201文字以上の場合は無効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "a".repeat(201),
          body: "本文",
          list: null,
        });
        expect(result.success).toBe(false);
      });

      it("bodyが空文字列の場合は無効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "ヘッドライン",
          body: "",
          list: null,
        });
        expect(result.success).toBe(false);
      });

      it("listに空文字列を含む場合は無効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "ヘッドライン",
          body: "本文",
          list: ["項目1", "", "項目3"],
        });
        expect(result.success).toBe(false);
      });

      it("headlineが欠けている場合は無効", () => {
        const result = privacyPolicySection.safeParse({
          body: "本文",
          list: null,
        });
        expect(result.success).toBe(false);
      });

      it("bodyが欠けている場合は無効", () => {
        const result = privacyPolicySection.safeParse({
          headline: "ヘッドライン",
          list: null,
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = privacyPolicySection.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = privacyPolicySection.safeParse(undefined);
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validatePrivacyPolicySection", () => {
    it("有効なUnvalidatedPrivacyPolicySectionでokを返す", () => {
      const result = validatePrivacyPolicySection({
        headline: "テストヘッドライン",
        body: "テスト本文です。",
        list: ["項目1", "項目2"],
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedPrivacyPolicySectionでerrを返す", () => {
      const result = validatePrivacyPolicySection({
        headline: "",
        body: "",
        list: null,
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("privacyPolicySchema", () => {
    describe("有効なPrivacyPolicyの検証", () => {
      it("Forgerで生成したPrivacyPolicyは有効", () => {
        const policy = Forger(PrivacyPolicyMold).forge();
        const result = privacyPolicySchema.safeParse(policy);
        expect(result.success).toBe(true);
      });

      it("sectionsが空配列でも有効", () => {
        const result = privacyPolicySchema.safeParse({
          sections: [],
          timeline: Forger(TimelineMold).forge(),
        });
        expect(result.success).toBe(true);
      });

      it("複数のsectionsを持つ場合も有効", () => {
        const result = privacyPolicySchema.safeParse({
          sections: Forger(PrivacyPolicySectionMold).forgeMulti(5),
          timeline: Forger(TimelineMold).forge(),
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なPrivacyPolicyの検証", () => {
      it("sectionsが欠けている場合は無効", () => {
        const result = privacyPolicySchema.safeParse({
          timeline: Forger(TimelineMold).forge(),
        });
        expect(result.success).toBe(false);
      });

      it("timelineが欠けている場合は無効", () => {
        const result = privacyPolicySchema.safeParse({
          sections: [],
        });
        expect(result.success).toBe(false);
      });

      it("timelineが無効な場合は無効", () => {
        const createdAt = new Date("2024-01-02");
        const updatedAt = new Date("2024-01-01");
        const result = privacyPolicySchema.safeParse({
          sections: [],
          timeline: { createdAt, updatedAt },
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = privacyPolicySchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = privacyPolicySchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validatePrivacyPolicy", () => {
    it("有効なUnvalidatedPrivacyPolicyでokを返す", () => {
      const now = new Date();
      const result = validatePrivacyPolicy({
        sections: [
          {
            headline: "プライバシーポリシー",
            body: "本ポリシーは...",
            list: null,
          },
        ],
        timeline: {
          createdAt: now,
          updatedAt: now,
        },
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedPrivacyPolicyでerrを返す", () => {
      const result = validatePrivacyPolicy({
        sections: [
          {
            headline: "",
            body: "",
            list: null,
          },
        ],
        timeline: {
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("siteDocumentSchema", () => {
    describe("有効なSiteDocumentの検証", () => {
      it("Forgerで生成したSiteDocumentは有効", () => {
        const document = Forger(SiteDocumentMold).forge();
        const result = siteDocumentSchema.safeParse(document);
        expect(result.success).toBe(true);
      });

      it("privacyが有効な場合は検証を通過する", () => {
        const result = siteDocumentSchema.safeParse({
          privacy: Forger(PrivacyPolicyMold).forge(),
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSiteDocumentの検証", () => {
      it("privacyが欠けている場合は無効", () => {
        const result = siteDocumentSchema.safeParse({});
        expect(result.success).toBe(false);
      });

      it("privacyが無効な場合は無効", () => {
        const result = siteDocumentSchema.safeParse({
          privacy: {
            sections: [],
          },
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = siteDocumentSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = siteDocumentSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateSiteDocument", () => {
    it("有効なSiteDocumentでokを返す", () => {
      const document = Forger(SiteDocumentMold).forge();
      const result = validateSiteDocument(document);
      expect(result.isOk).toBe(true);
    });

    it("無効なSiteDocumentでerrを返す", () => {
      const result = validateSiteDocument({});
      expect(result.isErr).toBe(true);
    });
  });

  describe("updatePrivacy", () => {
    it("有効なUnvalidatedPrivacyPolicyでSiteDocumentを作成する", () => {
      const now = new Date();
      const result = updatePrivacy({
        sections: [
          {
            headline: "更新されたポリシー",
            body: "新しい内容...",
            list: null,
          },
        ],
        timeline: {
          createdAt: now,
          updatedAt: now,
        },
      });
      expect(result.isOk).toBe(true);
      if (result.isOk) {
        expect(result.unwrap().privacy.sections).toHaveLength(1);
        expect(result.unwrap().privacy.sections[0].headline).toBe(
          "更新されたポリシー"
        );
      }
    });

    it("無効なUnvalidatedPrivacyPolicyでerrを返す", () => {
      const result = updatePrivacy({
        sections: [
          {
            headline: "",
            body: "",
            list: null,
          },
        ],
        timeline: {
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      });
      expect(result.isErr).toBe(true);
    });
  });
});
