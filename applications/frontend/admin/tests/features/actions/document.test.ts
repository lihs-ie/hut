/**
 * Document Actions Feature Test
 *
 * Firebase Emulatorを使用してドキュメントアクションの統合テストを行います。
 * Server Actionは`revalidatePath`などNext.js固有機能を使用するため、
 * ワークフローのコアロジックを直接テストします。
 */

import { describe, it, expect, beforeEach, afterAll } from "vitest";
import {
  createAdminFeatureTestContext,
  cleanupAdminFeatureTest,
  testLogger,
} from "../setup";
import {
  PrivacyPolicy,
  UnvalidatedPrivacyPolicy,
} from "@shared/domains/document/privacy";
import {
  createGetPrivacyPolicyWorkflow,
  createPrivacyPolicyPersistWorkflow,
} from "@shared/workflows/document";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SiteDocumentMold,
  PrivacyPolicySectionMold,
  SiteDocumentProperties,
} from "@shared-tests/support/molds/domains/document/common";
import { isValidationError } from "@shared/aspects/error";
import { FirebaseSiteDocumentRepository } from "@shared/infrastructures/documents";
import {
  getTestFirestoreInstance,
  createFirestoreOperations,
  cleanupTestApp,
  Timestamp,
} from "../firebase-test-utils";
import { doc, setDoc, type Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";

const TEST_APP_NAME = "document-feature-test-app";

type SuccessResult<T> = { success: true; data: T };
type FailureResult<E> = { success: false; error: E };
type ResultOutcome<T, E> = SuccessResult<T> | FailureResult<E>;

describe("Feature: Document Actions (実DB接続)", () => {
  let firestore: Firestore;
  let operations: FirestoreOperations;
  let documentRepository: ReturnType<typeof FirebaseSiteDocumentRepository>;

  const buildSiteDocument = (overrides?: Partial<SiteDocumentProperties>) =>
    Forger(SiteDocumentMold).forge(overrides);

  beforeEach(async () => {
    await createAdminFeatureTestContext();
    firestore = getTestFirestoreInstance(TEST_APP_NAME);
    operations = createFirestoreOperations();
    documentRepository = FirebaseSiteDocumentRepository(firestore, operations);
  }, 30000);

  afterAll(async () => {
    await cleanupAdminFeatureTest();
    await cleanupTestApp(TEST_APP_NAME);
  }, 30000);

  async function seedSiteDocument(
    unvalidatedPrivacyPolicy: UnvalidatedPrivacyPolicy,
  ): Promise<void> {
    const docRef = doc(firestore, "site-documents", "site-document");
    await setDoc(docRef, {
      privacy: {
        sections: unvalidatedPrivacyPolicy.sections,
        timeline: {
          createdAt: Timestamp.fromDate(
            unvalidatedPrivacyPolicy.timeline.createdAt,
          ),
          updatedAt: Timestamp.fromDate(
            unvalidatedPrivacyPolicy.timeline.updatedAt,
          ),
        },
      },
      version: 1,
    });
    documentRepository = FirebaseSiteDocumentRepository(firestore, operations);
  }

  describe("プライバシーポリシー永続化ワークフロー", () => {
    it("プライバシーポリシーを永続化できることを確認", async () => {
      const siteDocument = buildSiteDocument();
      const pastDate = new Date("2020-01-01T00:00:00.000Z");

      const unvalidatedPrivacyPolicy: UnvalidatedPrivacyPolicy = {
        sections: siteDocument.privacy.sections.map((section) => ({
          headline: section.headline,
          body: section.body,
          list: section.list,
        })),
        timeline: {
          createdAt: pastDate,
          updatedAt: pastDate,
        },
      };

      await seedSiteDocument(unvalidatedPrivacyPolicy);

      const persistWorkflow = createPrivacyPolicyPersistWorkflow(
        documentRepository.find,
      )(documentRepository.persist)(testLogger);

      const updatedSections = Forger(PrivacyPolicySectionMold).forgeMulti(2);

      const persistResult = await persistWorkflow({
        payload: {
          sections: updatedSections.map((section) => ({
            headline: section.headline,
            body: section.body,
            list: section.list,
          })),
        },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(persistResult.success).toBe(true);
    });

    it("永続化後に取得できることを確認", async () => {
      const siteDocument = buildSiteDocument();

      const unvalidatedPrivacyPolicy: UnvalidatedPrivacyPolicy = {
        sections: siteDocument.privacy.sections.map((section) => ({
          headline: section.headline,
          body: section.body,
          list: section.list,
        })),
        timeline: {
          createdAt: siteDocument.privacy.timeline.createdAt,
          updatedAt: siteDocument.privacy.timeline.updatedAt,
        },
      };

      await seedSiteDocument(unvalidatedPrivacyPolicy);

      const findWorkflow = createGetPrivacyPolicyWorkflow(
        documentRepository.find,
      )(testLogger);

      const findResult = await findWorkflow({
        payload: null,
        now: new Date(),
      }).match<ResultOutcome<PrivacyPolicy, unknown>>({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(true);
      if (findResult.success) {
        expect(findResult.data.sections.length).toBe(
          unvalidatedPrivacyPolicy.sections.length,
        );
        expect(findResult.data.sections[0].headline).toBe(
          unvalidatedPrivacyPolicy.sections[0].headline,
        );
      }
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なデータでValidationErrorが返る", async () => {
      const siteDocument = buildSiteDocument();

      const unvalidatedPrivacyPolicy: UnvalidatedPrivacyPolicy = {
        sections: siteDocument.privacy.sections.map((section) => ({
          headline: section.headline,
          body: section.body,
          list: section.list,
        })),
        timeline: {
          createdAt: siteDocument.privacy.timeline.createdAt,
          updatedAt: siteDocument.privacy.timeline.updatedAt,
        },
      };

      await seedSiteDocument(unvalidatedPrivacyPolicy);

      const persistWorkflow = createPrivacyPolicyPersistWorkflow(
        documentRepository.find,
      )(documentRepository.persist)(testLogger);

      const result = await persistWorkflow({
        payload: {
          sections: [
            {
              headline: "",
              body: "",
              list: null,
            },
          ],
        },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
        if (Array.isArray(result.errors)) {
          expect(result.errors.length).toBeGreaterThan(0);
          expect(result.errors.every((error) => isValidationError(error))).toBe(
            true,
          );
        }
      }
    });
  });
});
