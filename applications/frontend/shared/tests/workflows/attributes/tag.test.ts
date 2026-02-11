import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createTagFindWorkflow,
  createTagOfIdentifiersWorkflow,
  createTagSearchWorkflow,
  createTagPersistWorkflow,
  createTagTerminateWorkflow,
  createTagOfNamesWorkflow,
} from "@shared/workflows/attributes/tag";
import {
  validateTagIdentifier,
  validateTagIdentifiers,
  validateCriteria,
  validateTag,
  validateTagNames,
} from "@shared/domains/attributes/tag";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import { aggregateNotFoundError, unexpectedError } from "@shared/aspects/error";
import {
  TagMold,
  TagIdentifierMold,
} from "../../support/molds/domains/attributes/tag";
import { Command } from "@shared/workflows/common";

describe("workflows/attributes/tag", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createTagFindWorkflow", () => {
    it("有効なidentifierでタグを取得できる", async () => {
      const tag = Forger(TagMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(tag).toAsync());

      const workflow = createTagFindWorkflow(validateTagIdentifier)(findMock)(
        mockLogger
      );

      const command: Command<string> = {
        now: new Date(),
        payload: tag.identifier,
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(tag);
      expect(findMock).toHaveBeenCalledWith(tag.identifier);
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const findMock = vi.fn();

      const workflow = createTagFindWorkflow(validateTagIdentifier)(findMock)(
        mockLogger
      );

      const command: Command<string> = {
        now: new Date(),
        payload: "invalid-identifier",
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(findMock).not.toHaveBeenCalled();
    });

    it("タグが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(TagIdentifierMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError("Tag", "Tag not found");
      const findMock = vi.fn().mockReturnValue(err(notFoundError).toAsync());

      const workflow = createTagFindWorkflow(validateTagIdentifier)(findMock)(
        mockLogger
      );

      const command: Command<string> = {
        now: new Date(),
        payload: identifier,
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });

  describe("createTagOfIdentifiersWorkflow", () => {
    it("有効なidentifiersでタグ一覧を取得できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(3, 1);
      const identifiers = tags.map((tag) => tag.identifier);
      const ofIdentifiersMock = vi.fn().mockReturnValue(ok(tags).toAsync());

      const workflow = createTagOfIdentifiersWorkflow(validateTagIdentifiers)(
        ofIdentifiersMock
      )(mockLogger);

      const result = await workflow(identifiers).unwrap();

      expect(result).toEqual(tags);
      expect(ofIdentifiersMock).toHaveBeenCalledWith(identifiers);
    });

    it("無効なidentifiersでValidationErrorを返す", async () => {
      const ofIdentifiersMock = vi.fn();

      const workflow = createTagOfIdentifiersWorkflow(validateTagIdentifiers)(
        ofIdentifiersMock
      )(mockLogger);

      const result = workflow(["invalid", "identifiers"]);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(ofIdentifiersMock).not.toHaveBeenCalled();
    });

    it("空の配列でも検索できる", async () => {
      const ofIdentifiersMock = vi.fn().mockReturnValue(ok([]).toAsync());

      const workflow = createTagOfIdentifiersWorkflow(validateTagIdentifiers)(
        ofIdentifiersMock
      )(mockLogger);

      const result = await workflow([]).unwrap();

      expect(result).toEqual([]);
    });
  });

  describe("createTagSearchWorkflow", () => {
    it("有効な検索条件でタグを検索できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(3, 1);
      const searchMock = vi.fn().mockReturnValue(ok(tags).toAsync());

      const workflow = createTagSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<{ name: string | null }> = {
        now: new Date(),
        payload: { name: "test" },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(tags);
      expect(searchMock).toHaveBeenCalled();
    });

    it("nullの検索条件でも検索できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(5, 1);
      const searchMock = vi.fn().mockReturnValue(ok(tags).toAsync());

      const workflow = createTagSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<{ name: null }> = {
        now: new Date(),
        payload: { name: null },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(tags);
    });

    it("検索でエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Search failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createTagSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<{ name: null }> = {
        now: new Date(),
        payload: { name: null },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createTagPersistWorkflow", () => {
    it("有効なタグデータでタグを作成できる", async () => {
      const tag = Forger(TagMold).forgeWithSeed(1);
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createTagPersistWorkflow(validateTag)(persistMock)(
        mockLogger
      );

      const command: Command<{
        identifier: string;
        name: string;
        logo: string;
        timeline: { createdAt: Date; updatedAt: Date };
      }> = {
        now: new Date(),
        payload: {
          identifier: tag.identifier,
          name: tag.name,
          logo: tag.logo,
          timeline: tag.timeline,
        },
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("tag.persisted");
      expect(result.payload.identifier).toBe(tag.identifier);
      expect(persistMock).toHaveBeenCalled();
    });

    it("無効なタグデータでValidationErrorを返す", async () => {
      const persistMock = vi.fn();

      const workflow = createTagPersistWorkflow(validateTag)(persistMock)(
        mockLogger
      );

      const command: Command<{
        identifier: string;
        name: string;
        logo: string;
        timeline: { createdAt: Date; updatedAt: Date };
      }> = {
        now: new Date(),
        payload: {
          identifier: "invalid",
          name: "", // 空の名前は無効
          logo: "test-logo",
          timeline: { createdAt: new Date(), updatedAt: new Date() },
        },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化でエラーが発生した場合はエラーを返す", async () => {
      const tag = Forger(TagMold).forgeWithSeed(1);
      const error = unexpectedError("Persist failed");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createTagPersistWorkflow(validateTag)(persistMock)(
        mockLogger
      );

      const command: Command<{
        identifier: string;
        name: string;
        logo: string;
        timeline: { createdAt: Date; updatedAt: Date };
      }> = {
        now: new Date(),
        payload: {
          identifier: tag.identifier,
          name: tag.name,
          logo: tag.logo,
          timeline: tag.timeline,
        },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createTagTerminateWorkflow", () => {
    it("有効なidentifierでタグを削除できる", async () => {
      const identifier = Forger(TagIdentifierMold).forgeWithSeed(1);
      const terminateMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createTagTerminateWorkflow(validateTagIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<string> = {
        now: new Date(),
        payload: identifier,
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("tag.terminated");
      expect(terminateMock).toHaveBeenCalled();
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const terminateMock = vi.fn();

      const workflow = createTagTerminateWorkflow(validateTagIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<string> = {
        now: new Date(),
        payload: "invalid-identifier",
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(terminateMock).not.toHaveBeenCalled();
    });

    it("タグが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(TagIdentifierMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError("Tag", "Tag not found");
      const terminateMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createTagTerminateWorkflow(validateTagIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<string> = {
        now: new Date(),
        payload: identifier,
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });

  describe("createTagOfNamesWorkflow", () => {
    it("有効なnamesでタグ一覧を取得できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(3, 1);
      const names = tags.map((tag) => tag.name);
      const ofNamesMock = vi.fn().mockReturnValue(ok(tags).toAsync());

      const workflow = createTagOfNamesWorkflow(validateTagNames)(ofNamesMock)(
        mockLogger
      );

      const result = await workflow(names).unwrap();

      expect(result).toEqual(tags);
      expect(ofNamesMock).toHaveBeenCalledWith(names);
    });

    it("無効なnamesでValidationErrorを返す", async () => {
      const ofNamesMock = vi.fn();

      const workflow = createTagOfNamesWorkflow(validateTagNames)(ofNamesMock)(
        mockLogger
      );

      // 21文字以上の名前は無効
      const result = workflow([
        "this-is-a-very-long-name-that-exceeds-limit",
      ]);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(ofNamesMock).not.toHaveBeenCalled();
    });

    it("空の配列でも検索できる", async () => {
      const ofNamesMock = vi.fn().mockReturnValue(ok([]).toAsync());

      const workflow = createTagOfNamesWorkflow(validateTagNames)(ofNamesMock)(
        mockLogger
      );

      const result = await workflow([]).unwrap();

      expect(result).toEqual([]);
    });

    it("検索でエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Search failed");
      const ofNamesMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createTagOfNamesWorkflow(validateTagNames)(ofNamesMock)(
        mockLogger
      );

      const result = await workflow(["test"]).unwrapError();

      expect(result).toEqual(error);
    });
  });
});
