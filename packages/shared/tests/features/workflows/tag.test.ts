import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createTagPersistWorkflow,
  createTagTerminateWorkflow,
  createTagFindWorkflow,
  createTagSearchWorkflow,
} from "@shared/workflows/attributes/tag";
import {
  validateTag,
  validateTagIdentifier,
  validateCriteria,
} from "@shared/domains/attributes/tag";
import { FirebaseTagRepository } from "@shared/infrastructures/tags";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import {
  TagMold,
  TagIdentifierMold,
  TagNameMold,
} from "../../support/molds/domains/attributes/tag";
import type { Tag, TagRepository } from "@shared/domains/attributes/tag";

function toTagPayload(tag: Tag) {
  return {
    identifier: tag.identifier,
    name: tag.name,
    logo: tag.logo,
    timeline: tag.timeline,
  };
}

describe("Feature: Tag Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let repository: TagRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    repository = FirebaseTagRepository(context.firestore, context.operations);
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("タグ作成から検索までの一連の流れ", () => {
    it("新規タグを作成し、そのタグを検索して取得できる", async () => {
      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        repository.persist,
      )(testLogger);
      const searchWorkflow = createTagSearchWorkflow(validateCriteria)(
        repository.search,
      )(testLogger);

      const tag = Forger(TagMold).forgeWithSeed(1);

      const persistResult = await persistWorkflow({
        now: new Date(),
        payload: toTagPayload(tag),
      }).unwrap();

      expect(persistResult.type).toBe("tag.persisted");
      expect(persistResult.payload.identifier).toBe(tag.identifier);
      expect(persistResult.occurredAt).toBeDefined();

      const searchResult = await searchWorkflow({
        now: new Date(),
        payload: { name: null },
      }).unwrap();

      expect(searchResult.length).toBeGreaterThanOrEqual(1);
      expect(
        searchResult.some((item) => item.identifier === tag.identifier),
      ).toBe(true);
    });

    it("タグを作成後、identifierで取得できる", async () => {
      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        repository.persist,
      )(testLogger);
      const findWorkflow = createTagFindWorkflow(validateTagIdentifier)(
        repository.find,
      )(testLogger);

      const tag = Forger(TagMold).forgeWithSeed(2);

      await persistWorkflow({
        now: new Date(),
        payload: toTagPayload(tag),
      }).unwrap();

      const result = await findWorkflow({
        now: new Date(),
        payload: tag.identifier,
      }).unwrap();

      expect(result.identifier).toBe(tag.identifier);
      expect(result.name).toBe(tag.name);
      expect(result.logo).toBe(tag.logo);
    });
  });

  describe("タグ検索ワークフロー", () => {
    it("identifierでタグを取得できる", async () => {
      const tag = Forger(TagMold).forgeWithSeed(10);

      await repository.persist(tag).unwrap();

      const findWorkflow = createTagFindWorkflow(validateTagIdentifier)(
        repository.find,
      )(testLogger);

      const result = await findWorkflow({
        now: new Date(),
        payload: tag.identifier,
      }).unwrap();

      expect(result.identifier).toBe(tag.identifier);
      expect(result.name).toBe(tag.name);
      expect(result.logo).toBe(tag.logo);
    });

    it("名前の部分一致でタグを検索できる", async () => {
      const tagName = Forger(TagNameMold).forgeWithSeed(20, {
        value: "TestSearchTag",
      });
      const tag = Forger(TagMold).forgeWithSeed(21, { name: tagName });

      await repository.persist(tag).unwrap();

      const searchWorkflow = createTagSearchWorkflow(validateCriteria)(
        repository.search,
      )(testLogger);

      const result = await searchWorkflow({
        now: new Date(),
        payload: { name: "TestSearch" },
      }).unwrap();

      expect(result.length).toBe(1);
      expect(result[0]?.identifier).toBe(tag.identifier);
    });

    it("検索条件なしで全てのタグを取得できる", async () => {
      const tag1 = Forger(TagMold).forgeWithSeed(30);
      const tag2 = Forger(TagMold).forgeWithSeed(31);

      await repository.persist(tag1).unwrap();
      await repository.persist(tag2).unwrap();

      const searchWorkflow = createTagSearchWorkflow(validateCriteria)(
        repository.search,
      )(testLogger);

      const result = await searchWorkflow({
        now: new Date(),
        payload: { name: null },
      }).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(2);
      expect(result.some((item) => item.identifier === tag1.identifier)).toBe(
        true,
      );
      expect(result.some((item) => item.identifier === tag2.identifier)).toBe(
        true,
      );
    });
  });

  describe("タグ削除ワークフロー", () => {
    it("タグを削除でき、その後検索で見つからない", async () => {
      const tag = Forger(TagMold).forgeWithSeed(50);

      await repository.persist(tag).unwrap();

      const terminateWorkflow = createTagTerminateWorkflow(
        validateTagIdentifier,
      )(repository.terminate)(testLogger);
      const findWorkflow = createTagFindWorkflow(validateTagIdentifier)(
        repository.find,
      )(testLogger);

      const terminateResult = await terminateWorkflow({
        now: new Date(),
        payload: tag.identifier,
      }).unwrap();

      expect(terminateResult.type).toBe("tag.terminated");
      expect(terminateResult.payload.identifier).toBe(tag.identifier);
      expect(terminateResult.occurredAt).toBeDefined();

      const findResult = await findWorkflow({
        now: new Date(),
        payload: tag.identifier,
      }).match({
        ok: () => "found",
        err: () => "not-found",
      });

      expect(findResult).toBe("not-found");
    });
  });

  describe("並列処理の最適化", () => {
    it("複数タグの作成が並列で実行できる", async () => {
      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        repository.persist,
      )(testLogger);

      const tags = Forger(TagMold).forgeMultiWithSeed(3, 60);

      const results = await Promise.all(
        tags.map((tag) =>
          persistWorkflow({
            now: new Date(),
            payload: toTagPayload(tag),
          }).unwrap(),
        ),
      );

      expect(results.length).toBe(3);
      results.forEach((result, index) => {
        expect(result.payload.identifier).toBe(tags[index]?.identifier);
      });

      // 永続化されたことを確認
      for (const tag of tags) {
        const found = await repository.find(tag.identifier).unwrap();
        expect(found.identifier).toBe(tag.identifier);
      }
    });
  });

  describe("エラーハンドリング", () => {
    it("存在しないタグを検索するとAggregateNotFoundErrorが返る", async () => {
      const findWorkflow = createTagFindWorkflow(validateTagIdentifier)(
        repository.find,
      )(testLogger);

      const nonExistentIdentifier =
        Forger(TagIdentifierMold).forgeWithSeed(100);

      const result = await findWorkflow({
        now: new Date(),
        payload: nonExistentIdentifier,
      }).match({
        ok: () => ({ found: true }),
        err: (error) => ({ found: false, error }),
      });

      expect(result.found).toBe(false);
    });

    it("無効なidentifierでValidationErrorが返る", async () => {
      const findWorkflow = createTagFindWorkflow(validateTagIdentifier)(
        repository.find,
      )(testLogger);

      const result = await findWorkflow({
        now: new Date(),
        payload: "invalid-identifier",
      }).match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
    });
  });

  describe("データ永続化の検証", () => {
    it("作成したタグのデータが正確に永続化される", async () => {
      const tag = Forger(TagMold).forgeWithSeed(110);

      await repository.persist(tag).unwrap();

      const found = await repository.find(tag.identifier).unwrap();

      expect(found.identifier).toBe(tag.identifier);
      expect(found.name).toBe(tag.name);
      expect(found.logo).toBe(tag.logo);
      expect(found.timeline.createdAt.getTime()).toBe(
        tag.timeline.createdAt.getTime(),
      );
      expect(found.timeline.updatedAt.getTime()).toBe(
        tag.timeline.updatedAt.getTime(),
      );
    });

    it("タグの更新が正しく永続化される", async () => {
      const tag = Forger(TagMold).forgeWithSeed(120);

      await repository.persist(tag).unwrap();

      const updatedName = Forger(TagNameMold).forgeWithSeed(121, {
        value: "UpdatedTag",
      });
      const updatedTag = {
        ...tag,
        name: updatedName,
        timeline: {
          ...tag.timeline,
          updatedAt: new Date(),
        },
      };

      const validatedUpdatedTag = validateTag(updatedTag).unwrap();

      await repository.persist(validatedUpdatedTag).unwrap();

      const found = await repository.find(tag.identifier).unwrap();

      expect(found.identifier).toBe(tag.identifier);
      expect(found.name).toBe(updatedName);
    });
  });
});
