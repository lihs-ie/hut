/**
 * Document Workflow Feature Test
 *
 * Firebase Emulatorを使用してDocumentワークフローの統合テストを行います。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createGetPrivacyPolicyWorkflow,
  createPrivacyPolicyPersistWorkflow,
} from "@shared/workflows/document";
import { FirebaseSiteDocumentRepository } from "@shared/infrastructures/documents";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import {
  SiteDocumentMold,
  PrivacyPolicySectionMold,
  PrivacyPolicyMold,
} from "../../support/molds/domains/document/common";
import type { SiteDocumentRepository } from "@shared/domains/document";

describe("Feature: Document Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let repository: SiteDocumentRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    repository = FirebaseSiteDocumentRepository(
      context.firestore,
      context.operations
    );
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("プライバシーポリシー取得ワークフロー", () => {
    it("永続化されたプライバシーポリシーを取得できる", async () => {
      const siteDocument = Forger(SiteDocumentMold).forgeWithSeed(1);

      await repository.persist(siteDocument).unwrap();

      const getPrivacyPolicyWorkflow = createGetPrivacyPolicyWorkflow(
        repository.find
      )(testLogger);

      const result = await getPrivacyPolicyWorkflow({
        now: new Date(),
        payload: null,
      }).unwrap();

      expect(result.sections.length).toBe(siteDocument.privacy.sections.length);
      expect(result.sections[0]?.headline).toBe(
        siteDocument.privacy.sections[0]?.headline
      );
      expect(result.sections[0]?.body).toBe(
        siteDocument.privacy.sections[0]?.body
      );
    });
  });

  describe("プライバシーポリシー永続化ワークフロー", () => {
    it("プライバシーポリシーを更新できる", async () => {
      const initialDocument = Forger(SiteDocumentMold).forgeWithSeed(10);
      await repository.persist(initialDocument).unwrap();

      const persistWorkflow = createPrivacyPolicyPersistWorkflow(
        repository.find
      )(repository.persist)(testLogger);

      const newSections = Forger(PrivacyPolicySectionMold).forgeMultiWithSeed(
        5,
        11
      );

      const unvalidatedSections = newSections.map((section) => ({
        headline: section.headline,
        body: section.body,
        list: section.list,
      }));

      await persistWorkflow({
        now: new Date(),
        payload: { sections: unvalidatedSections },
      }).unwrap();

      const getWorkflow = createGetPrivacyPolicyWorkflow(repository.find)(
        testLogger
      );

      const result = await getWorkflow({
        now: new Date(),
        payload: null,
      }).unwrap();

      expect(result.sections.length).toBe(5);
      expect(result.sections[0]?.headline).toBe(newSections[0]?.headline);
      expect(result.sections[0]?.body).toBe(newSections[0]?.body);
    });

    it("セクションの追加と削除ができる", async () => {
      const initialDocument = Forger(SiteDocumentMold).forgeWithSeed(20);
      await repository.persist(initialDocument).unwrap();

      const persistWorkflow = createPrivacyPolicyPersistWorkflow(
        repository.find
      )(repository.persist)(testLogger);

      const additionalSections =
        Forger(PrivacyPolicySectionMold).forgeMultiWithSeed(2, 21);
      const unvalidatedSections = [
        ...initialDocument.privacy.sections.slice(0, 1).map((section) => ({
          headline: section.headline,
          body: section.body,
          list: section.list,
        })),
        ...additionalSections.map((section) => ({
          headline: section.headline,
          body: section.body,
          list: section.list,
        })),
      ];

      await persistWorkflow({
        now: new Date(),
        payload: { sections: unvalidatedSections },
      }).unwrap();

      const getWorkflow = createGetPrivacyPolicyWorkflow(repository.find)(
        testLogger
      );

      const result = await getWorkflow({
        now: new Date(),
        payload: null,
      }).unwrap();

      expect(result.sections.length).toBe(3);
      expect(result.sections[0]?.headline).toBe(
        initialDocument.privacy.sections[0]?.headline
      );
      expect(result.sections[1]?.headline).toBe(additionalSections[0]?.headline);
      expect(result.sections[2]?.headline).toBe(additionalSections[1]?.headline);
    });
  });

  describe("データ永続化の検証", () => {
    it("プライバシーポリシーのセクションデータが正確に永続化される", async () => {
      const siteDocument = Forger(SiteDocumentMold).forgeWithSeed(30);

      await repository.persist(siteDocument).unwrap();

      const found = await repository.find().unwrap();

      expect(found.privacy.sections.length).toBe(
        siteDocument.privacy.sections.length
      );
      siteDocument.privacy.sections.forEach((section, index) => {
        const foundSection = found.privacy.sections[index];
        expect(foundSection?.headline).toBe(section.headline);
        expect(foundSection?.body).toBe(section.body);
        expect(foundSection?.list).toEqual(section.list);
      });
      expect(found.privacy.timeline.createdAt.getTime()).toBe(
        siteDocument.privacy.timeline.createdAt.getTime()
      );
      expect(found.privacy.timeline.updatedAt.getTime()).toBe(
        siteDocument.privacy.timeline.updatedAt.getTime()
      );
    });

    it("リスト付きセクションとリストなしセクションの両方が永続化される", async () => {
      const sectionWithList = Forger(PrivacyPolicySectionMold).forgeWithSeed(
        40,
        {
          list: ["Item 1", "Item 2", "Item 3"],
        }
      );
      const sectionWithoutList = Forger(PrivacyPolicySectionMold).forgeWithSeed(
        41,
        {
          list: null,
        }
      );

      const privacy = Forger(PrivacyPolicyMold).forgeWithSeed(42, {
        sections: [sectionWithList, sectionWithoutList],
      });

      const siteDocument = Forger(SiteDocumentMold).forgeWithSeed(43, {
        privacy,
      });

      await repository.persist(siteDocument).unwrap();

      const found = await repository.find().unwrap();

      expect(found.privacy.sections[0]?.list).toEqual(["Item 1", "Item 2", "Item 3"]);
      expect(found.privacy.sections[1]?.list).toBeNull();
    });
  });

  describe("エラーハンドリング", () => {
    it("ドキュメントが存在しない場合はAggregateNotFoundErrorが返る", async () => {
      const getWorkflow = createGetPrivacyPolicyWorkflow(repository.find)(
        testLogger
      );

      const result = await getWorkflow({
        now: new Date(),
        payload: null,
      }).match({
        ok: () => ({ found: true }),
        err: (error) => ({ found: false, error }),
      });

      expect(result.found).toBe(false);
    });
  });

  describe("タイムラインの更新", () => {
    it("更新時にupdatedAtが更新される", async () => {
      const initialDocument = Forger(SiteDocumentMold).forgeWithSeed(50);
      await repository.persist(initialDocument).unwrap();

      const persistWorkflow = createPrivacyPolicyPersistWorkflow(
        repository.find
      )(repository.persist)(testLogger);

      const newSections = Forger(PrivacyPolicySectionMold).forgeMultiWithSeed(
        2,
        51
      );

      const unvalidatedSections = newSections.map((section) => ({
        headline: section.headline,
        body: section.body,
        list: section.list,
      }));

      await persistWorkflow({
        now: new Date(),
        payload: { sections: unvalidatedSections },
      }).unwrap();

      const getWorkflow = createGetPrivacyPolicyWorkflow(repository.find)(
        testLogger
      );

      const result = await getWorkflow({
        now: new Date(),
        payload: null,
      }).unwrap();

      expect(result.timeline.createdAt.getTime()).toBe(
        initialDocument.privacy.timeline.createdAt.getTime()
      );
      expect(result.timeline.updatedAt.getTime()).toBeGreaterThanOrEqual(
        initialDocument.privacy.timeline.updatedAt.getTime()
      );
    });
  });
});
