import { describe, it, expect } from "vitest";
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

const parseTechnologyCategory = (category: TechnologyCategory): BrandedTechnologyCategory =>
  technologyCategorySchema.parse(category);

type TechnologyStackMap = Map<BrandedTechnologyCategory, TechnologyStack[]>;

describe("infrastructures/admin (with mock repository)", () => {
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
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [] });
        const admin = Forger(AdminMold).forgeWithSeed(1);

        const result = await repository.persist(admin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.identifier).toBe(admin.identifier);
        expect(found.profile.name).toBe(admin.profile.name);
      });

      it("既存の管理者を更新できる", async () => {
        const admin = Forger(AdminMold).forgeWithSeed(2);
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

        const newName = Forger(AdminNameMold).forgeWithSeed(100, {
          value: "UpdatedName",
        });
        const newProfile = Forger(ProfileMold).forgeWithSeed(0, {
          ...admin.profile,
          name: newName,
        });
        const updatedAdmin = Forger(AdminMold).forgeWithSeed(0, {
          ...admin,
          profile: newProfile,
        });

        const result = await repository.persist(updatedAdmin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.profile.name).toBe(newName);
      });

      it("プロフィール全体を更新できる", async () => {
        const admin = Forger(AdminMold).forgeWithSeed(3);
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

        const newEmail = Forger(MailAddressMold).forgeWithSeed(101, {
          value: "new-email@example.com",
        });
        const newProfile = Forger(ProfileMold).forgeWithSeed(0, {
          ...admin.profile,
          email: newEmail,
          bio: "Updated bio text",
        });
        const updatedAdmin = Forger(AdminMold).forgeWithSeed(0, {
          ...admin,
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
        const initialProfile = Forger(ProfileMold).forgeWithSeed(4, {
          careers: initialCareers,
        });
        const admin = Forger(AdminMold).forgeWithSeed(5, {
          profile: initialProfile,
        });
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

        const newCareers = Forger(CareerMold).forgeMultiWithSeed(3, 20);
        const newProfile = Forger(ProfileMold).forgeWithSeed(0, {
          ...admin.profile,
          careers: newCareers,
        });
        const updatedAdmin = Forger(AdminMold).forgeWithSeed(0, {
          ...admin,
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
        const initialProfile = Forger(ProfileMold).forgeWithSeed(6, {
          externalServices: initialServices,
        });
        const admin = Forger(AdminMold).forgeWithSeed(7, {
          profile: initialProfile,
        });
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

        const newServices = Forger(ExternalServiceMold).forgeMultiWithSeed(
          3,
          40,
        );
        const newProfile = Forger(ProfileMold).forgeWithSeed(0, {
          ...admin.profile,
          externalServices: newServices,
        });
        const updatedAdmin = Forger(AdminMold).forgeWithSeed(0, {
          ...admin,
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
        const initialProfile = Forger(ProfileMold).forgeWithSeed(8, {
          techStacks: initialTechStacks,
        });
        const admin = Forger(AdminMold).forgeWithSeed(9, {
          profile: initialProfile,
        });
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

        const newTechStacks = createTechnologyStackMap({
          [TechnologyCategory.FRONTEND]: { count: 3, seed: 60 },
          [TechnologyCategory.BACKEND]: { count: 2, seed: 70 },
        });
        const newProfile = Forger(ProfileMold).forgeWithSeed(0, {
          ...admin.profile,
          techStacks: newTechStacks,
        });
        const updatedAdmin = Forger(AdminMold).forgeWithSeed(0, {
          ...admin,
          profile: newProfile,
        });

        const result = await repository.persist(updatedAdmin).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.profile.techStacks.size).toBe(2);
      });

      it("onPersistコールバックが呼ばれる", async () => {
        let persistedAdmin: Admin | undefined;
        const admin = Forger(AdminMold).forgeWithSeed(10);
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, {
          instancies: [],
          onPersist: (callbackAdmin) => {
            persistedAdmin = callbackAdmin;
          },
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
        const admin = Forger(AdminMold).forgeWithSeed(20);
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

        const found = await repository.find().unwrap();

        expect(found.identifier).toBe(admin.identifier);
        expect(found.profile.name).toBe(admin.profile.name);
        expect(found.profile.email).toBe(admin.profile.email);
        expect(found.profile.bio).toBe(admin.profile.bio);
      });

      it("存在しない管理者を取得しようとするとエラーになる", async () => {
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [] });

        const error = await extractError(repository.find());

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("キャリア情報も含めて取得できる", async () => {
        const careers = Forger(CareerMold).forgeMultiWithSeed(3, 80);
        const profile = Forger(ProfileMold).forgeWithSeed(21, { careers });
        const admin = Forger(AdminMold).forgeWithSeed(22, { profile });
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

        const found = await repository.find().unwrap();

        expect(found.profile.careers.length).toBe(3);
      });

      it("外部サービス情報も含めて取得できる", async () => {
        const externalServices = Forger(ExternalServiceMold).forgeMultiWithSeed(
          2,
          90,
        );
        const profile = Forger(ProfileMold).forgeWithSeed(23, { externalServices });
        const admin = Forger(AdminMold).forgeWithSeed(24, { profile });
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

        const found = await repository.find().unwrap();

        expect(found.profile.externalServices.length).toBe(2);
      });

      it("技術スタック情報も含めて取得できる", async () => {
        const techStacks = createTechnologyStackMap({
          [TechnologyCategory.FRONTEND]: { count: 2, seed: 100 },
          [TechnologyCategory.BACKEND]: { count: 3, seed: 110 },
        });
        const profile = Forger(ProfileMold).forgeWithSeed(25, { techStacks });
        const admin = Forger(AdminMold).forgeWithSeed(26, { profile });
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [admin] });

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

        const profile = Forger(ProfileMold).forgeWithSeed(50, {
          careers,
          externalServices,
          techStacks,
        });
        const admin = Forger(AdminMold).forgeWithSeed(51, { profile });
        const repository = Forger(AdminRepositoryMold).forgeWithSeed(1, { instancies: [] });

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
