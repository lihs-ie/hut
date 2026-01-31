/**
 * Admin Workflow Feature Test
 *
 * Firebase Emulatorを使用してAdminワークフローの統合テストを行います。
 * uploadAvatarはテスト用スタブ実装を使用（ファイルストレージ接続は対象外）
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createAdminFindWorkflow,
  createPersistProfileWorkflow,
} from "@shared/workflows/admin";
import {
  Admin,
  AdminRepository,
  Profile,
  validateProfile,
} from "@shared/domains/user";
import { FirebaseAdminRepository } from "@shared/infrastructures/admin";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import { AdminMold, ProfileMold } from "../../support/molds/domains/user";
import { ok, type AsyncResult } from "@shared/aspects/result";
import type {
  AggregateNotFoundError,
  UnexpectedError,
} from "@shared/aspects/error";
import type { Image } from "@shared/domains/common/image";
import type { Command } from "@shared/workflows/common";

type AdminFindWorkflow = (
  command: Command<null>
) => AsyncResult<Admin, AggregateNotFoundError<"Admin"> | UnexpectedError>;

/**
 * テスト用のuploadAvatar実装
 * 実際のファイルストレージにはアクセスせず、入力されたavatarをそのまま返す
 */
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

describe("Feature: Admin Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let repository: AdminRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    repository = FirebaseAdminRepository(context.firestore, context.operations);
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("管理者検索ワークフロー", () => {
    it("永続化された管理者を取得できる", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(1);

      await repository.persist(admin).unwrap();

      const findWorkflow = createAdminFindWorkflow(testLogger)(repository.find);

      const result = await findWorkflow({
        now: new Date(),
        payload: null,
      }).unwrap();

      expect(result.identifier).toBe(admin.identifier);
      expect(result.profile.name).toBe(admin.profile.name);
      expect(result.profile.email).toBe(admin.profile.email);
    });

    it("管理者が存在しない場合はAggregateNotFoundErrorが返る", async () => {
      const findWorkflow = createAdminFindWorkflow(testLogger)(repository.find);

      const result = await findWorkflow({
        now: new Date(),
        payload: null,
      }).match({
        ok: () => ({ found: true }),
        err: (error) => ({ found: false, error }),
      });

      expect(result.found).toBe(false);
    });
  });

  describe("プロファイル永続化ワークフロー", () => {
    it("プロファイルを更新できる", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(10);
      await repository.persist(admin).unwrap();

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(repository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(repository.persist);

      const newProfile = Forger(ProfileMold).forgeWithSeed(11);

      await persistWorkflow({
        now: new Date(),
        payload: createProfilePayload(newProfile),
      }).unwrap();

      const findWorkflow = createAdminFindWorkflow(testLogger)(repository.find);
      const result = await findWorkflow({
        now: new Date(),
        payload: null,
      }).unwrap();

      expect(result.identifier).toBe(admin.identifier);
      expect(result.profile.name).toBe(newProfile.name);
      expect(result.profile.email).toBe(newProfile.email);
      expect(result.profile.bio).toBe(newProfile.bio);
    });

    it("プロファイルの各フィールドが正しく更新される", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(20);
      await repository.persist(admin).unwrap();

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(repository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(repository.persist);

      const newProfile = Forger(ProfileMold).forgeWithSeed(21, {
        bio: "Updated bio content for testing",
      });

      await persistWorkflow({
        now: new Date(),
        payload: createProfilePayload(newProfile),
      }).unwrap();

      const findWorkflow = createAdminFindWorkflow(testLogger)(repository.find);
      const result = await findWorkflow({
        now: new Date(),
        payload: null,
      }).unwrap();

      expect(result.profile.bio).toBe("Updated bio content for testing");
      expect(result.profile.careers.length).toBe(newProfile.careers.length);
      expect(result.profile.externalServices.length).toBe(
        newProfile.externalServices.length
      );
    });
  });

  describe("データ永続化の検証", () => {
    it("管理者のプロファイルデータが正確に永続化される", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(30);

      await repository.persist(admin).unwrap();

      const found = await repository.find().unwrap();

      expect(found.identifier).toBe(admin.identifier);
      expect(found.profile.name).toBe(admin.profile.name);
      expect(found.profile.email).toBe(admin.profile.email);
      expect(found.profile.bio).toBe(admin.profile.bio);
      expect(found.profile.avatar).toBe(admin.profile.avatar);
      expect(found.profile.careers.length).toBe(admin.profile.careers.length);
      expect(found.profile.externalServices.length).toBe(
        admin.profile.externalServices.length
      );
    });

    it("キャリア情報が正確に永続化される", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(40);

      await repository.persist(admin).unwrap();

      const found = await repository.find().unwrap();

      admin.profile.careers.forEach((career, index) => {
        const foundCareer = found.profile.careers[index];
        expect(foundCareer?.company).toBe(career.company);
        expect(foundCareer?.role).toBe(career.role);
        expect(foundCareer?.description).toBe(career.description);
        expect(foundCareer?.period.from.getTime()).toBe(
          career.period.from.getTime()
        );
        if (career.period.to !== null) {
          expect(foundCareer?.period.to?.getTime()).toBe(
            career.period.to.getTime()
          );
        } else {
          expect(foundCareer?.period.to).toBeNull();
        }
      });
    });

    it("外部サービス情報が正確に永続化される", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(50);

      await repository.persist(admin).unwrap();

      const found = await repository.find().unwrap();

      admin.profile.externalServices.forEach((service, index) => {
        const foundService = found.profile.externalServices[index];
        expect(foundService?.type).toBe(service.type);
        expect(foundService?.user).toBe(service.user);
      });
    });
  });

  describe("エラーハンドリング", () => {
    it("管理者が存在しない状態でプロファイル更新を試みるとAggregateNotFoundErrorが返る", async () => {
      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(repository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(repository.persist);

      const profile = Forger(ProfileMold).forgeWithSeed(60);

      const result = await persistWorkflow({
        now: new Date(),
        payload: createProfilePayload(profile),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(result.success).toBe(false);
    });

    it("無効なプロファイルデータでValidationErrorが返る", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(70);
      await repository.persist(admin).unwrap();

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(repository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(repository.persist);

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
        now: new Date(),
        payload: invalidPayload,
      }).match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
    });
  });

  describe("連続操作", () => {
    it("管理者の作成と更新が連続して実行できる", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(80);
      await repository.persist(admin).unwrap();

      const uploadAvatar = createTestUploadAvatar();
      const adminFindWorkflow: AdminFindWorkflow = createAdminFindWorkflow(
        testLogger
      )(repository.find);

      const persistWorkflow = createPersistProfileWorkflow(validateProfile)(
        uploadAvatar
      )(testLogger)(adminFindWorkflow)(repository.persist);

      const firstUpdateProfile = Forger(ProfileMold).forgeWithSeed(81);
      await persistWorkflow({
        now: new Date(),
        payload: createProfilePayload(firstUpdateProfile),
      }).unwrap();

      const secondUpdateProfile = Forger(ProfileMold).forgeWithSeed(82);
      await persistWorkflow({
        now: new Date(),
        payload: createProfilePayload(secondUpdateProfile),
      }).unwrap();

      const findWorkflow = createAdminFindWorkflow(testLogger)(repository.find);
      const result = await findWorkflow({
        now: new Date(),
        payload: null,
      }).unwrap();

      expect(result.profile.name).toBe(secondUpdateProfile.name);
      expect(result.profile.email).toBe(secondUpdateProfile.email);
    });
  });
});
