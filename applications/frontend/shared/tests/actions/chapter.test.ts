/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  ChapterIdentifierMold,
} from "../support/molds/domains/series/chapter";
import { SlugMold } from "../support/molds/domains/common/slug";
import { aggregateNotFoundError } from "@shared/aspects/error";
import {
  setupUnwrapWithResolvedValue,
  setupUnwrapWithRejectedValue,
} from "../support/helpers";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

vi.mock("@shared/providers/workflows/chapter", () => ({
  ChapterWorkflowProvider: {
    findBySlug: vi.fn(),
  },
}));

vi.mock("@shared/providers/infrastructure/chapter", () => ({
  ChapterRepositoryProvider: {
    firebase: {
      ofIdentifiers: vi.fn(),
    },
  },
}));

describe("chapter actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  describe("findChapterBySlug", () => {
    it("slugでChapterを正常に取得できる", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(chapter);

      const { findChapterBySlug } = await import("@shared/actions/chapter");
      const result = await findChapterBySlug(chapter.slug);

      expect(result).toEqual(chapter);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("Chapterが見つからない場合はエラーがスローされる", async () => {
      const slug = Forger(SlugMold).forgeWithSeed(1);
      await setupUnwrapWithRejectedValue(
        aggregateNotFoundError("Chapter", "Chapter not found."),
      );

      const { findChapterBySlug } = await import("@shared/actions/chapter");

      await expect(findChapterBySlug(slug)).rejects.toThrow();
    });
  });

  describe("findChaptersByIdentifiers", () => {
    it("identifiers配列でChapter一覧を正常に取得できる", async () => {
      const chapterList = Forger(ChapterMold).forgeMultiWithSeed(3, 1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(chapterList);

      const { findChaptersByIdentifiers } = await import(
        "@shared/actions/chapter"
      );
      const identifiers = chapterList.map((chapter) => chapter.identifier);
      const result = await findChaptersByIdentifiers(identifiers);

      expect(result).toEqual(chapterList);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("空のidentifiers配列を渡すと空配列を返す", async () => {
      const unwrapForNextJs = await setupUnwrapWithResolvedValue([]);

      const { findChaptersByIdentifiers } = await import(
        "@shared/actions/chapter"
      );
      const result = await findChaptersByIdentifiers([]);

      expect(result).toEqual([]);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("Chapterが見つからない場合はエラーがスローされる", async () => {
      const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(1);
      await setupUnwrapWithRejectedValue(
        aggregateNotFoundError("Chapter", "Chapter not found."),
      );

      const { findChaptersByIdentifiers } = await import(
        "@shared/actions/chapter"
      );

      await expect(
        findChaptersByIdentifiers([identifier]),
      ).rejects.toThrow();
    });
  });
});
