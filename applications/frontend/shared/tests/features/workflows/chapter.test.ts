import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { createChapterFindBySlugWorkflow } from "@shared/workflows/chapter";
import { createPassthroughFilter } from "@shared/workflows/common";
import { validateSlug } from "@shared/domains/common/slug";
import { FirebaseChapterRepository } from "@shared/infrastructures/chapter";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import {
  ChapterMold,
} from "../../support/molds/domains/series/chapter";
import { SlugMold } from "../../support/molds/domains/common/slug";
import type { ChapterRepository } from "@shared/domains/series/chapter";

describe("Feature: Chapter Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let repository: ChapterRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    repository = FirebaseChapterRepository(
      context.firestore,
      context.operations,
    );
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("slugでChapterを取得するワークフロー", () => {
    it("正常系: slugに対応するChapterを取得できる", async () => {
      const targetSlug = Forger(SlugMold).forgeWithSeed(10, {
        value: "feature-test-chapter-slug",
      });
      const chapter = Forger(ChapterMold).forgeWithSeed(11, {
        slug: targetSlug,
      });

      await repository.persist(chapter).unwrap();

      const findBySlugWorkflow = createChapterFindBySlugWorkflow(validateSlug)(
        testLogger,
      )(repository.findBySlug)(createPassthroughFilter());

      const result = await findBySlugWorkflow({
        now: new Date(),
        payload: { slug: targetSlug },
      }).unwrap();

      expect(result.identifier).toBe(chapter.identifier);
      expect(result.slug).toBe(targetSlug);
      expect(result.title).toBe(chapter.title);
      expect(result.content).toBe(chapter.content);
    });

    it("異常系: 存在しないslugで検索するとエラーになる", async () => {
      const nonExistentSlug = Forger(SlugMold).forgeWithSeed(20, {
        value: "non-existent-chapter-slug-feature",
      });

      const findBySlugWorkflow = createChapterFindBySlugWorkflow(validateSlug)(
        testLogger,
      )(repository.findBySlug)(createPassthroughFilter());

      const result = await findBySlugWorkflow({
        now: new Date(),
        payload: { slug: nonExistentSlug },
      }).match({
        ok: () => ({ found: true }),
        err: (error) => ({ found: false, error }),
      });

      expect(result.found).toBe(false);
    });

    it("正常系: imagesとstatusが正確に永続化される", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(30);

      await repository.persist(chapter).unwrap();

      const findBySlugWorkflow = createChapterFindBySlugWorkflow(validateSlug)(
        testLogger,
      )(repository.findBySlug)(createPassthroughFilter());

      const result = await findBySlugWorkflow({
        now: new Date(),
        payload: { slug: chapter.slug },
      }).unwrap();

      expect(result.images).toEqual(chapter.images);
      expect(result.status).toBe(chapter.status);
    });
  });
});
