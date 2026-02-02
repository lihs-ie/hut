/**
 * Profile Actions Feature Test
 *
 * Firebase Emulatorを使用してプロファイルアクションの統合テストを行います。
 * Server Actionは`revalidateTag`などNext.js固有機能を使用するため、
 * ワークフローのコアロジックを直接テストします。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import {
  createAdminFeatureTestContext,
  cleanupAdminFeatureTest,
  testLogger,
} from "../setup";
import {
  Admin,
  AdminRepository,
  Profile,
  validateProfile,
} from "@shared/domains/user";
import {
  createAdminFindWorkflow,
  createPersistProfileWorkflow,
} from "@shared/workflows/admin";
import { Forger } from "@lihs-ie/forger-ts";
import { AdminMold, ProfileMold } from "@shared-tests/support/molds/domains/user";
import { isValidationError } from "@shared/aspects/error";
import { FirebaseAdminRepository } from "@shared/infrastructures/admin";
import {
  getTestFirestoreInstance,
  createFirestoreOperations,
  cleanupTestApp,
} from "../firebase-test-utils";
import { ok, type AsyncResult } from "@shared/aspects/result";
import type { UnexpectedError, AggregateNotFoundError } from "@shared/aspects/error";
import type { Image } from "@shared/domains/common/image";
import type { Command } from "@shared/workflows/common";
import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";

type SuccessResult<T> = { success: true; data: T };
type FailureResult<E> = { success: false; error: E };
type ResultOutcome<T, E> = SuccessResult<T> | FailureResult<E>;

const TEST_APP_NAME = "profile-feature-test-app";

type AdminFindWorkflow = (
  command: Command<null>
) => AsyncResult<Admin, AggregateNotFoundError<"Admin"> | UnexpectedError>;

function createTestUploadAvatar(): (
  avatar: string
) => AsyncResult<Image, UnexpectedError> {
  return (avatar: string) => ok(avatar as Image).toAsync();
}

function createProfilePayload(profile: Profile) {
  return {
    avatar: profile.avatar,
    name: profile.name,
    email: profile.email,
    careers: profile.careers,
    bio: profile.bio,
    externalServices: profile.externalServices,
    techStacks: profile.techStacks,
  };
}

describe("Feature: Profile Actions (実DB接続)", () => {
  let firestore: Firestore;
  let operations: FirestoreOperations;
  let adminRepository: AdminRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    await createAdminFeatureTestContext();
    firestore = getTestFirestoreInstance(TEST_APP_NAME);
    operations = createFirestoreOperations();
    adminRepository = FirebaseAdminRepository(firestore, operations);
  }, 30000);

  afterAll(async () => {
    await cleanupAdminFeatureTest();
    await cleanupTestApp(TEST_APP_NAME);
  }, 30000);

  describe("プロファイル永続化ワークフロー", () => {
    it("プロファイルを永続化できることを確認", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(1);
      await adminRepository.persist(admin).unwrap();

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(adminRepository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(adminRepository.persist);

      const newProfile = Forger(ProfileMold).forgeWithSeed(2);

      const persistResult = await persistWorkflow({
        payload: createProfilePayload(newProfile),
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(persistResult.success).toBe(true);
    });

    it("永続化後に取得できることを確認", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(10);
      await adminRepository.persist(admin).unwrap();

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(adminRepository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(adminRepository.persist);

      const newProfile = Forger(ProfileMold).forgeWithSeed(11);

      await persistWorkflow({
        payload: createProfilePayload(newProfile),
        now: new Date(),
      }).unwrap();

      const findWorkflow = createAdminFindWorkflow(testLogger)(
        adminRepository.find
      );

      const findResult = await findWorkflow({
        payload: null,
        now: new Date(),
      }).match<ResultOutcome<Admin, unknown>>({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(true);
      if (findResult.success && "data" in findResult) {
        const found = findResult.data;
        expect(found.identifier).toBe(admin.identifier);
        expect(found.profile.name).toBe(newProfile.name);
        expect(found.profile.email).toBe(newProfile.email);
        expect(found.profile.bio).toBe(newProfile.bio);
      }
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なデータでValidationErrorが返る", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(20);
      await adminRepository.persist(admin).unwrap();

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(adminRepository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(adminRepository.persist);

      const invalidPayload = {
        avatar: "",
        name: "",
        email: "invalid-email",
        careers: [],
        bio: "",
        externalServices: [],
        techStacks: new Map(),
      };

      const result = await persistWorkflow({
        payload: invalidPayload,
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
          expect(
            result.errors.every((error) => isValidationError(error))
          ).toBe(true);
        }
      }
    });

    it("空の名前でValidationErrorが返る", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(30);
      await adminRepository.persist(admin).unwrap();

      const profile = Forger(ProfileMold).forgeWithSeed(31);

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(adminRepository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(adminRepository.persist);

      const result = await persistWorkflow({
        payload: { ...createProfilePayload(profile), name: "" },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
      }
    });

    it("無効なメールアドレスでValidationErrorが返る", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(40);
      await adminRepository.persist(admin).unwrap();

      const profile = Forger(ProfileMold).forgeWithSeed(41);

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(adminRepository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(adminRepository.persist);

      const result = await persistWorkflow({
        payload: { ...createProfilePayload(profile), email: "invalid-email-format" },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
      }
    });

    it("長すぎる名前でValidationErrorが返る", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(50);
      await adminRepository.persist(admin).unwrap();

      const profile = Forger(ProfileMold).forgeWithSeed(51);

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(adminRepository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(adminRepository.persist);

      const result = await persistWorkflow({
        payload: { ...createProfilePayload(profile), name: "a".repeat(21) },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
      }
    });
  });

  describe("管理者未存在時のエラー", () => {
    it("管理者が存在しない状態でプロファイル更新を試みるとエラーが返る", async () => {
      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(adminRepository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(adminRepository.persist);

      const profile = Forger(ProfileMold).forgeWithSeed(60);

      const result = await persistWorkflow({
        payload: createProfilePayload(profile),
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(result.success).toBe(false);
    });
  });

  describe("連続更新", () => {
    it("プロファイルを連続して更新できる", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(70);
      await adminRepository.persist(admin).unwrap();

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(adminRepository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(adminRepository.persist);

      const firstProfile = Forger(ProfileMold).forgeWithSeed(71);
      await persistWorkflow({
        payload: createProfilePayload(firstProfile),
        now: new Date(),
      }).unwrap();

      const secondProfile = Forger(ProfileMold).forgeWithSeed(72);
      await persistWorkflow({
        payload: createProfilePayload(secondProfile),
        now: new Date(),
      }).unwrap();

      const findWorkflow = createAdminFindWorkflow(testLogger)(
        adminRepository.find
      );
      const result = await findWorkflow({
        payload: null,
        now: new Date(),
      }).unwrap();

      expect(result.profile.name).toBe(secondProfile.name);
      expect(result.profile.email).toBe(secondProfile.email);
    });
  });
});
