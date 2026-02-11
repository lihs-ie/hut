import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  adminIdentifierSchema,
  adminNameSchema,
  mailAddressSchema,
  roleSchema,
  Role,
  periodSchema,
  career,
  profileSchema,
  validateProfile,
  adminSchema,
  validateAdmin,
  updateProfile,
} from "@shared/domains/user";
import {
  AdminMold,
  AdminIdentifierMold,
  AdminNameMold,
  MailAddressMold,
  PeriodMold,
  CareerMold,
  ProfileMold,
} from "../../support/molds/domains/user";
import { ImageMold } from "../../support/molds/domains/common/image";
import { ExternalServiceMold } from "../../support/molds/domains/common/service";
import {
  describeIdentifierSchema,
  describeStringLengthSchema,
} from "../../support/helpers";

describe("domains/user/admin", () => {
  describe("adminIdentifierSchema", () => {
    describeIdentifierSchema(
      "AdminIdentifier",
      adminIdentifierSchema,
      () => Forger(AdminIdentifierMold).forge(),
      (count) => Forger(AdminIdentifierMold).forgeMulti(count)
    );
  });

  describe("adminNameSchema", () => {
    describeStringLengthSchema("管理者名", adminNameSchema, 1, 20);

    it("日本語を含む名前は有効", () => {
      const result = adminNameSchema.safeParse("山田太郎");
      expect(result.success).toBe(true);
    });
  });

  describe("mailAddressSchema", () => {
    describe("有効なメールアドレスの検証", () => {
      it("標準的なメールアドレスは有効", () => {
        const result = mailAddressSchema.safeParse("test@example.com");
        expect(result.success).toBe(true);
      });

      it("サブドメインを含むメールアドレスは有効", () => {
        const result = mailAddressSchema.safeParse("test@sub.example.com");
        expect(result.success).toBe(true);
      });

      it("プラス記号を含むメールアドレスは有効", () => {
        const result = mailAddressSchema.safeParse("test+tag@example.com");
        expect(result.success).toBe(true);
      });

      it("Forgerで生成したメールアドレスは有効", () => {
        const result = mailAddressSchema.safeParse(Forger(MailAddressMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効なメールアドレスの検証", () => {
      it("空文字列は無効", () => {
        const result = mailAddressSchema.safeParse("");
        expect(result.success).toBe(false);
      });

      it("@がないメールアドレスは無効", () => {
        const result = mailAddressSchema.safeParse("testexample.com");
        expect(result.success).toBe(false);
      });

      it("101文字以上のメールアドレスは無効", () => {
        const email = "a".repeat(92) + "@test.com";
        const result = mailAddressSchema.safeParse(email);
        expect(result.success).toBe(false);
      });
    });
  });

  describe("roleSchema", () => {
    describe("有効なロールの検証", () => {
      it("FULL_STACK roleは有効", () => {
        const result = roleSchema.safeParse(Role.FULL_STACK);
        expect(result.success).toBe(true);
      });
    });

    describe("無効なロールの検証", () => {
      it("存在しないロールは無効", () => {
        const result = roleSchema.safeParse("Invalid Role");
        expect(result.success).toBe(false);
      });

      it("空文字列は無効", () => {
        const result = roleSchema.safeParse("");
        expect(result.success).toBe(false);
      });
    });
  });

  describe("periodSchema", () => {
    describe("有効な期間の検証", () => {
      it("from と to が両方Dateの場合は有効", () => {
        const result = periodSchema.safeParse({
          from: new Date("2020-01-01"),
          to: new Date("2023-12-31"),
        });
        expect(result.success).toBe(true);
      });

      it("to が null でも有効（継続中）", () => {
        const result = periodSchema.safeParse({ from: new Date("2020-01-01"), to: null });
        expect(result.success).toBe(true);
      });

      it("Forgerで生成した期間は有効", () => {
        const result = periodSchema.safeParse(Forger(PeriodMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効な期間の検証", () => {
      it("from が欠けている場合は無効", () => {
        const result = periodSchema.safeParse({ to: new Date() });
        expect(result.success).toBe(false);
      });

      it("from が Date でない場合は無効", () => {
        const result = periodSchema.safeParse({ from: "2020-01-01", to: new Date() });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("career", () => {
    describe("有効なキャリアの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = career.safeParse(Forger(CareerMold).forge());
        expect(result.success).toBe(true);
      });

      it("descriptionが空文字でも有効", () => {
        const result = career.safeParse({
          company: "Test Company",
          period: Forger(PeriodMold).forge(),
          role: Role.FULL_STACK,
          description: "",
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なキャリアの検証", () => {
      const createCareerWithOverrides = (overrides: Record<string, unknown>) => ({
        company: "Test Company",
        period: Forger(PeriodMold).forge(),
        role: Role.FULL_STACK,
        description: "Description",
        ...overrides,
      });

      it("companyが空の場合は無効", () => {
        const result = career.safeParse(createCareerWithOverrides({ company: "" }));
        expect(result.success).toBe(false);
      });

      it("companyが51文字以上の場合は無効", () => {
        const result = career.safeParse(createCareerWithOverrides({ company: "a".repeat(51) }));
        expect(result.success).toBe(false);
      });

      it("descriptionが501文字以上の場合は無効", () => {
        const result = career.safeParse(createCareerWithOverrides({ description: "a".repeat(501) }));
        expect(result.success).toBe(false);
      });
    });
  });

  describe("profileSchema", () => {
    describe("有効なProfileの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = profileSchema.safeParse(Forger(ProfileMold).forge());
        expect(result.success).toBe(true);
      });

      it("careersが空配列でも有効", () => {
        const result = profileSchema.safeParse(Forger(ProfileMold).forge({ careers: [] }));
        expect(result.success).toBe(true);
      });

      it("externalServicesが空配列でも有効", () => {
        const result = profileSchema.safeParse(Forger(ProfileMold).forge({ externalServices: [] }));
        expect(result.success).toBe(true);
      });

      it("bioが空文字でも有効", () => {
        const result = profileSchema.safeParse(Forger(ProfileMold).forge({ bio: "" }));
        expect(result.success).toBe(true);
      });
    });

    describe("無効なProfileの検証", () => {
      const createProfileWithOverrides = (overrides: Record<string, unknown>) => ({
        avatar: Forger(ImageMold).forge(),
        name: Forger(AdminNameMold).forge(),
        email: Forger(MailAddressMold).forge(),
        careers: [],
        bio: "Bio",
        externalServices: [],
        techStacks: new Map(),
        ...overrides,
      });

      it("nameが空の場合は無効", () => {
        const result = profileSchema.safeParse(createProfileWithOverrides({ name: "" }));
        expect(result.success).toBe(false);
      });

      it("emailが無効な形式の場合は無効", () => {
        const result = profileSchema.safeParse(createProfileWithOverrides({ email: "invalid-email" }));
        expect(result.success).toBe(false);
      });

      it("bioが301文字以上の場合は無効", () => {
        const result = profileSchema.safeParse(createProfileWithOverrides({ bio: "a".repeat(301) }));
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateProfile", () => {
    it("有効なUnvalidatedProfileでokを返す", () => {
      const result = validateProfile({
        avatar: Forger(ImageMold).forge(),
        name: Forger(AdminNameMold).forge(),
        email: Forger(MailAddressMold).forge(),
        careers: [],
        bio: "自己紹介文",
        externalServices: Forger(ExternalServiceMold).forgeMulti(2),
        techStacks: new Map(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedProfileでerrを返す", () => {
      const result = validateProfile({
        avatar: "not-a-url",
        name: "",
        email: "invalid",
        careers: [],
        bio: "a".repeat(301),
        externalServices: [],
        techStacks: new Map(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("adminSchema", () => {
    describe("有効なAdminの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = adminSchema.safeParse(Forger(AdminMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効なAdminの検証", () => {
      it("identifierが無効な場合は無効", () => {
        const result = adminSchema.safeParse({
          identifier: "invalid",
          profile: Forger(ProfileMold).forge(),
        });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateAdmin", () => {
    it("有効なUnvalidatedAdminでokを返す", () => {
      const result = validateAdmin({
        identifier: Forger(AdminIdentifierMold).forge(),
        profile: {
          avatar: Forger(ImageMold).forge(),
          name: Forger(AdminNameMold).forge(),
          email: Forger(MailAddressMold).forge(),
          careers: [],
          bio: "自己紹介",
          externalServices: [],
          techStacks: new Map(),
        },
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedAdminでerrを返す", () => {
      const result = validateAdmin({
        identifier: "invalid",
        profile: {
          avatar: "not-a-url",
          name: "",
          email: "invalid",
          careers: [],
          bio: "",
          externalServices: [],
          techStacks: new Map(),
        },
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("updateProfile", () => {
    it("Adminのプロフィールを更新する", () => {
      const admin = Forger(AdminMold).forge();
      const newName = Forger(AdminNameMold).forge();
      const newEmail = Forger(MailAddressMold).forge();

      const result = updateProfile(admin, {
        avatar: Forger(ImageMold).forge(),
        name: newName,
        email: newEmail,
        careers: [],
        bio: "新しい自己紹介",
        externalServices: [],
        techStacks: new Map(),
      });

      expect(result.isOk).toBe(true);
      if (result.isOk) {
        const updatedAdmin = result.unwrap();
        expect(updatedAdmin.identifier).toBe(admin.identifier);
        expect(updatedAdmin.profile.name).toBe(newName);
        expect(updatedAdmin.profile.email).toBe(newEmail);
        expect(updatedAdmin.profile.bio).toBe("新しい自己紹介");
      }
    });

    it("無効なプロフィールで更新しようとするとerrを返す", () => {
      const admin = Forger(AdminMold).forge();
      const result = updateProfile(admin, {
        avatar: "not-a-url",
        name: "",
        email: "invalid",
        careers: [],
        bio: "a".repeat(301),
        externalServices: [],
        techStacks: new Map(),
      });
      expect(result.isErr).toBe(true);
    });
  });
});
