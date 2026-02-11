import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createAdminFindWorkflow,
  createPersistProfileWorkflow,
} from "@shared/workflows/admin";
import { Admin, Profile, validateProfile } from "@shared/domains/user";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err, type AsyncResult } from "@shared/aspects/result";
import { aggregateNotFoundError, unexpectedError, type AggregateNotFoundError, type UnexpectedError } from "@shared/aspects/error";
import { AdminMold, ProfileMold } from "../support/molds/domains/user";
import { ImageMold } from "../support/molds/domains/common/image";
import type { Image } from "@shared/domains/common/image";
import type { Command } from "@shared/workflows/common";

type UploadAvatar = (avatar: string) => AsyncResult<Image, UnexpectedError>;
type AdminFindWorkflow = (
  command: Command<null>,
) => AsyncResult<Admin, AggregateNotFoundError<"Admin"> | UnexpectedError>;
type PersistAdmin = (
  admin: Admin,
) => AsyncResult<void, UnexpectedError | AggregateNotFoundError<"Admin">>;

describe("workflows/admin", () => {
  const logger = Logger(Environment.DEVELOPMENT);

  const createCommand = <T>(payload: T) => ({
    now: new Date(),
    payload,
  });

  const createProfilePayload = (profile: Profile) => ({
    avatar: profile.avatar,
    name: profile.name,
    email: profile.email,
    careers: profile.careers,
    bio: profile.bio,
    externalServices: profile.externalServices,
    techStacks: profile.techStacks,
  });

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createAdminFindWorkflow", () => {
    it("管理者を正常に取得できる", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(admin).toAsync());
      const workflow = createAdminFindWorkflow(logger)(findMock);

      const result = await workflow(createCommand(null)).unwrap();

      expect(result).toEqual(admin);
      expect(findMock).toHaveBeenCalled();
    });

    it("管理者が見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const notFoundError = aggregateNotFoundError("Admin", "Admin not found");
      const findMock = vi.fn().mockReturnValue(err(notFoundError).toAsync());
      const workflow = createAdminFindWorkflow(logger)(findMock);

      const result = await workflow(createCommand(null)).unwrapError();

      expect(result).toEqual(notFoundError);
    });

    it("予期しないエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Database connection failed");
      const findMock = vi.fn().mockReturnValue(err(error).toAsync());
      const workflow = createAdminFindWorkflow(logger)(findMock);

      const result = await workflow(createCommand(null)).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createPersistProfileWorkflow", () => {
    const createWorkflow = (
      uploadAvatarMock: UploadAvatar,
      findMock: AdminFindWorkflow,
      persistMock: PersistAdmin
    ) =>
      createPersistProfileWorkflow(validateProfile)(uploadAvatarMock)(logger)(
        findMock
      )(persistMock);

    it("有効なプロファイルで更新できる", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(1);
      const profile = Forger(ProfileMold).forgeWithSeed(2);
      const image = Forger(ImageMold).forgeWithSeed(3);

      const uploadAvatarMock = vi.fn().mockReturnValue(ok(image).toAsync());
      const findMock = vi.fn().mockReturnValue(ok(admin).toAsync());
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());
      const workflow = createWorkflow(uploadAvatarMock, findMock, persistMock);

      const result = await workflow(
        createCommand(createProfilePayload(profile))
      ).match({
        ok: () => true,
        err: () => false,
      });

      expect(result).toBe(true);
      expect(uploadAvatarMock).toHaveBeenCalled();
      expect(findMock).toHaveBeenCalled();
      expect(persistMock).toHaveBeenCalled();
    });

    it("バリデーションエラーが発生した場合はValidationErrorを返す", async () => {
      const uploadAvatarMock = vi.fn();
      const findMock = vi.fn();
      const persistMock = vi.fn();
      const workflow = createWorkflow(uploadAvatarMock, findMock, persistMock);

      const invalidPayload = {
        avatar: "",
        name: "",
        email: "invalid-email",
        careers: [],
        bio: "",
        externalServices: [],
        techStacks: new Map(),
      };

      const result = await workflow(createCommand(invalidPayload)).match({
        ok: () => false,
        err: () => true,
      });

      expect(result).toBe(true);
      expect(uploadAvatarMock).not.toHaveBeenCalled();
    });

    it("アバターアップロードでエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(1);
      const profile = Forger(ProfileMold).forgeWithSeed(1);
      const error = unexpectedError("Upload failed");

      const uploadAvatarMock = vi.fn().mockReturnValue(err(error).toAsync());
      const findMock = vi.fn().mockReturnValue(ok(admin).toAsync());
      const persistMock = vi.fn();
      const workflow = createWorkflow(uploadAvatarMock, findMock, persistMock);

      const result = await workflow(
        createCommand(createProfilePayload(profile))
      ).unwrapError();

      expect(result).toEqual(error);
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("管理者が見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const profile = Forger(ProfileMold).forgeWithSeed(1);
      const image = Forger(ImageMold).forgeWithSeed(2);
      const notFoundError = aggregateNotFoundError("Admin", "Admin not found");

      const uploadAvatarMock = vi.fn().mockReturnValue(ok(image).toAsync());
      const findMock = vi.fn().mockReturnValue(err(notFoundError).toAsync());
      const persistMock = vi.fn();
      const workflow = createWorkflow(uploadAvatarMock, findMock, persistMock);

      const result = await workflow(
        createCommand(createProfilePayload(profile))
      ).unwrapError();

      expect(result).toEqual(notFoundError);
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化でエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(1);
      const profile = Forger(ProfileMold).forgeWithSeed(2);
      const image = Forger(ImageMold).forgeWithSeed(3);
      const error = unexpectedError("Persist failed");

      const uploadAvatarMock = vi.fn().mockReturnValue(ok(image).toAsync());
      const findMock = vi.fn().mockReturnValue(ok(admin).toAsync());
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());
      const workflow = createWorkflow(uploadAvatarMock, findMock, persistMock);

      const result = await workflow(
        createCommand(createProfilePayload(profile))
      ).unwrapError();

      expect(result).toEqual(error);
    });
  });
});
