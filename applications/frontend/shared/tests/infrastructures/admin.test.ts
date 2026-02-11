import { describe, it, expect } from "vitest";
import { Builder } from "../support/molds";
import {
  AdminMold,
  AdminRepositoryMold,
  ProfileMold,
  CareerMold,
  AdminNameMold,
  MailAddressMold,
} from "../support/molds/domains/user/common";
import { isAggregateNotFoundError } from "@shared/aspects/error";
import { Forger } from "@lihs-ie/forger-ts";
import { TechnologyStackMold } from "../support/molds/domains/common/tech";
import { TechnologyCategory, technologyCategorySchema, type TechnologyStack } from "@shared/domains/common/tech";
import { ExternalServiceMold } from "../support/molds/domains/common/service";
import { extractError } from "../support/helpers";
import type { Admin } from "@shared/domains/user";

type BrandedTechnologyCategory = ReturnType<typeof technologyCategorySchema.parse>;

// Branded型のカテゴリを作成するヘルパー
const parseTechnologyCategory = (category: TechnologyCategory): BrandedTechnologyCategory =>
  technologyCategorySchema.parse(category);

type TechnologyStackMap = Map<BrandedTechnologyCategory, TechnologyStack[]>;

describe("infrastructures/admin (with mock repository)", () => {
  const createRepository = (
    admins: Admin[] = [],
    onPersist?: (admin: Admin) => void,
  ) =>
    Forger(AdminRepositoryMold).forgeWithSeed(1, {
      instancies: admins,
      onPersist,
    });

  const createTechnologyStackMap = (
    stacks: Partial<Record<TechnologyCategory, { count: number; seed: number }>>,
  ): TechnologyStackMap => {
    const map: TechnologyStackMap = new Map();
    for (const [category, config] of Object.entries(stacks)) {
      if (config) {
        map.set(
          parseTechnologyCategory(category as TechnologyCategory),
          Forger(TechnologyStackMold).forgeMultiWithSeed(config.count, config.seed),
        );
      }
    }
    return map;
  };

  describe("AdminRepository", () => {
    describe("persist", () => {
      it("新しい管理者を保存できる", async () => {
        const repository = createRepository();
        const admin = Builder(AdminMold).buildWith(1);

        const result = await repository.persist(admin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.identifier).toBe(admin.identifier);
        expect(found.profile.name).toBe(admin.profile.name);
      });

      it("既存の管理者を更新できる", async () => {
        const admin = Builder(AdminMold).buildWith(2);
        const repository = createRepository([admin]);

        const newName = Builder(AdminNameMold).buildWith(100, {
          value: "UpdatedName",
        });
        const newProfile = Builder(ProfileMold).duplicate(admin.profile, {
          name: newName,
        });
        const updatedAdmin = Builder(AdminMold).duplicate(admin, {
          profile: newProfile,
        });

        const result = await repository.persist(updatedAdmin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.profile.name).toBe(newName);
      });

      it("プロフィール全体を更新できる", async () => {
        const admin = Builder(AdminMold).buildWith(3);
        const repository = createRepository([admin]);

        const newEmail = Builder(MailAddressMold).buildWith(101, {
          value: "new-email@example.com",
        });
        const newProfile = Builder(ProfileMold).duplicate(admin.profile, {
          email: newEmail,
          bio: "Updated bio text",
        });
        const updatedAdmin = Builder(AdminMold).duplicate(admin, {
          profile: newProfile,
        });

        const result = await repository.persist(updatedAdmin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.profile.email).toBe(newEmail);
        expect(found.profile.bio).toBe("Updated bio text");
      });

      it("キャリア情報を更新できる", async () => {
        const initialCareers = Forger(CareerMold).forgeMultiWithSeed(2, 10);
        const initialProfile = Builder(ProfileMold).buildWith(4, {
          careers: initialCareers,
        });
        const admin = Builder(AdminMold).buildWith(5, {
          profile: initialProfile,
        });
        const repository = createRepository([admin]);

        const newCareers = Forger(CareerMold).forgeMultiWithSeed(3, 20);
        const newProfile = Builder(ProfileMold).duplicate(admin.profile, {
          careers: newCareers,
        });
        const updatedAdmin = Builder(AdminMold).duplicate(admin, {
          profile: newProfile,
        });

        const result = await repository.persist(updatedAdmin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.profile.careers.length).toBe(3);
      });

      it("外部サービス情報を更新できる", async () => {
        const initialServices = Forger(ExternalServiceMold).forgeMultiWithSeed(
          1,
          30,
        );
        const initialProfile = Builder(ProfileMold).buildWith(6, {
          externalServices: initialServices,
        });
        const admin = Builder(AdminMold).buildWith(7, {
          profile: initialProfile,
        });
        const repository = createRepository([admin]);

        const newServices = Forger(ExternalServiceMold).forgeMultiWithSeed(
          3,
          40,
        );
        const newProfile = Builder(ProfileMold).duplicate(admin.profile, {
          externalServices: newServices,
        });
        const updatedAdmin = Builder(AdminMold).duplicate(admin, {
          profile: newProfile,
        });

        const result = await repository.persist(updatedAdmin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.profile.externalServices.length).toBe(3);
      });

      it("技術スタック情報を更新できる", async () => {
        const initialTechStacks = createTechnologyStackMap({
          [TechnologyCategory.FRONTEND]: { count: 2, seed: 50 },
        });
        const initialProfile = Builder(ProfileMold).buildWith(8, {
          techStacks: initialTechStacks,
        });
        const admin = Builder(AdminMold).buildWith(9, {
          profile: initialProfile,
        });
        const repository = createRepository([admin]);

        const newTechStacks = createTechnologyStackMap({
          [TechnologyCategory.FRONTEND]: { count: 3, seed: 60 },
          [TechnologyCategory.BACKEND]: { count: 2, seed: 70 },
        });
        const newProfile = Builder(ProfileMold).duplicate(admin.profile, {
          techStacks: newTechStacks,
        });
        const updatedAdmin = Builder(AdminMold).duplicate(admin, {
          profile: newProfile,
        });

        const result = await repository.persist(updatedAdmin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.profile.techStacks.size).toBe(2);
      });

      it("onPersistコールバックが呼ばれる", async () => {
        let persistedAdmin: Admin | undefined;
        const admin = Builder(AdminMold).buildWith(10);
        const repository = createRepository([], (callbackAdmin) => {
          persistedAdmin = callbackAdmin;
        });

        await repository.persist(admin).unwrap();

        expect(persistedAdmin).toBeDefined();
        if (persistedAdmin) {
          expect(persistedAdmin.identifier).toBe(admin.identifier);
        }
      });
    });

    describe("find", () => {
      it("存在する管理者を取得できる", async () => {
        const admin = Builder(AdminMold).buildWith(20);
        const repository = createRepository([admin]);

        const found = await repository.find().unwrap();

        expect(found.identifier).toBe(admin.identifier);
        expect(found.profile.name).toBe(admin.profile.name);
        expect(found.profile.email).toBe(admin.profile.email);
        expect(found.profile.bio).toBe(admin.profile.bio);
      });

      it("存在しない管理者を取得しようとするとエラーになる", async () => {
        const repository = createRepository();

        const error = await extractError(repository.find());

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("キャリア情報も含めて取得できる", async () => {
        const careers = Forger(CareerMold).forgeMultiWithSeed(3, 80);
        const profile = Builder(ProfileMold).buildWith(21, { careers });
        const admin = Builder(AdminMold).buildWith(22, { profile });
        const repository = createRepository([admin]);

        const found = await repository.find().unwrap();

        expect(found.profile.careers.length).toBe(3);
      });

      it("外部サービス情報も含めて取得できる", async () => {
        const externalServices = Forger(ExternalServiceMold).forgeMultiWithSeed(
          2,
          90,
        );
        const profile = Builder(ProfileMold).buildWith(23, { externalServices });
        const admin = Builder(AdminMold).buildWith(24, { profile });
        const repository = createRepository([admin]);

        const found = await repository.find().unwrap();

        expect(found.profile.externalServices.length).toBe(2);
      });

      it("技術スタック情報も含めて取得できる", async () => {
        const techStacks = createTechnologyStackMap({
          [TechnologyCategory.FRONTEND]: { count: 2, seed: 100 },
          [TechnologyCategory.BACKEND]: { count: 3, seed: 110 },
        });
        const profile = Builder(ProfileMold).buildWith(25, { techStacks });
        const admin = Builder(AdminMold).buildWith(26, { profile });
        const repository = createRepository([admin]);

        const found = await repository.find().unwrap();

        expect(found.profile.techStacks.size).toBe(2);
        expect(
          found.profile.techStacks.get(parseTechnologyCategory(TechnologyCategory.FRONTEND))?.length,
        ).toBe(2);
        expect(
          found.profile.techStacks.get(parseTechnologyCategory(TechnologyCategory.BACKEND))?.length,
        ).toBe(3);
      });
    });

    describe("complex profile data", () => {
      it("複雑なプロフィールデータを正しく保存・取得できる", async () => {
        const careers = Forger(CareerMold).forgeMultiWithSeed(5, 120);
        const externalServices = Forger(ExternalServiceMold).forgeMultiWithSeed(
          4,
          130,
        );
        const techStacks = createTechnologyStackMap({
          [TechnologyCategory.FRONTEND]: { count: 4, seed: 140 },
          [TechnologyCategory.BACKEND]: { count: 5, seed: 150 },
        });

        const profile = Builder(ProfileMold).buildWith(50, {
          careers,
          externalServices,
          techStacks,
        });
        const admin = Builder(AdminMold).buildWith(51, { profile });
        const repository = createRepository();

        await repository.persist(admin).unwrap();

        const found = await repository.find().unwrap();

        expect(found.profile.careers.length).toBe(5);
        expect(found.profile.externalServices.length).toBe(4);
        expect(found.profile.techStacks.size).toBe(2);
        expect(
          found.profile.techStacks.get(parseTechnologyCategory(TechnologyCategory.FRONTEND))?.length,
        ).toBe(4);
        expect(
          found.profile.techStacks.get(parseTechnologyCategory(TechnologyCategory.BACKEND))?.length,
        ).toBe(5);
      });
    });
  });
});
