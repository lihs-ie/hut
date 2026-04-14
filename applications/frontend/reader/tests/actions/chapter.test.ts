/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  ChapterIdentifierMold,
} from "../../../shared/tests/support/molds/domains/series/chapter";
import { SlugMold } from "../../../shared/tests/support/molds/domains/common/slug";
import { aggregateNotFoundError } from "@shared/aspects/error";
import { PublishStatus } from "@shared/domains/common";
import {
  setupUnwrapForAsyncResult,
  createSuccessAsyncResult,
  createErrorAsyncResult,
} from "../../../shared/tests/support/helpers";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

vi.mock("@/providers/workflows/chapter", () => ({
  ReaderChapterWorkflowProvider: {
    findBySlug: vi.fn(),
  },
}));

vi.mock("@/providers/infrastructure/firestore", () => ({
  ReaderFirestoreProvider: {
    chapterRepository: {
      find: vi.fn(),
      findBySlug: vi.fn(),
      ofIdentifiers: vi.fn(),
      persist: vi.fn(),
      terminate: vi.fn(),
    },
    articleRepository: {
      find: vi.fn(),
      findBySlug: vi.fn(),
      ofIdentifiers: vi.fn(),
      search: vi.fn(),
      persist: vi.fn(),
      terminate: vi.fn(),
    },
    memoRepository: {
      find: vi.fn(),
      findBySlug: vi.fn(),
      ofIdentifiers: vi.fn(),
      search: vi.fn(),
      persist: vi.fn(),
      terminate: vi.fn(),
    },
    seriesRepository: {
      find: vi.fn(),
      findBySlug: vi.fn(),
      ofIdentifiers: vi.fn(),
      search: vi.fn(),
      persist: vi.fn(),
      terminate: vi.fn(),
    },
  },
}));

describe("reader chapter actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  describe("findChapterBySlug", () => {
    it("公開済みChapterをslugで正常に取得できる", async () => {
      const chapter = Forger(ChapterMold).forge({
        status: PublishStatus.PUBLISHED,
      });
      await setupUnwrapForAsyncResult();

      const { ReaderChapterWorkflowProvider } = await import(
        "@/providers/workflows/chapter"
      );
      vi.mocked(ReaderChapterWorkflowProvider.findBySlug).mockReturnValue(
        createSuccessAsyncResult(chapter),
      );

      const { findChapterBySlug } = await import("@/actions/chapter");
      const result = await findChapterBySlug(chapter.slug);

      expect(result).toEqual(chapter);
    });

    it("下書きChapterはAggregateNotFoundErrorになる", async () => {
      const slug = Forger(SlugMold).forgeWithSeed(1);
      await setupUnwrapForAsyncResult();

      const { ReaderChapterWorkflowProvider } = await import(
        "@/providers/workflows/chapter"
      );
      vi.mocked(ReaderChapterWorkflowProvider.findBySlug).mockReturnValue(
        createErrorAsyncResult(
          aggregateNotFoundError("Chapter", "Chapter is not published"),
        ),
      );

      const { findChapterBySlug } = await import("@/actions/chapter");

      await expect(findChapterBySlug(slug)).rejects.toThrow();
    });

    it("Chapterが存在しない場合はエラーがスローされる", async () => {
      const slug = Forger(SlugMold).forgeWithSeed(2);
      await setupUnwrapForAsyncResult();

      const { ReaderChapterWorkflowProvider } = await import(
        "@/providers/workflows/chapter"
      );
      vi.mocked(ReaderChapterWorkflowProvider.findBySlug).mockReturnValue(
        createErrorAsyncResult(
          aggregateNotFoundError("Chapter", "Chapter not found."),
        ),
      );

      const { findChapterBySlug } = await import("@/actions/chapter");

      await expect(findChapterBySlug(slug)).rejects.toThrow();
    });
  });

  describe("findPublishedChaptersByIdentifiers", () => {
    it("識別子リストから公開済みChapterのみを返す", async () => {
      const publishedChapter = Forger(ChapterMold).forge({
        status: PublishStatus.PUBLISHED,
      });
      const draftChapter = Forger(ChapterMold).forge({
        status: PublishStatus.DRAFT,
      });
      const allChapters = [publishedChapter, draftChapter];
      const identifiers = allChapters.map((chapter) => chapter.identifier);

      await setupUnwrapForAsyncResult();

      const { ReaderFirestoreProvider } = await import(
        "@/providers/infrastructure/firestore"
      );
      vi.mocked(
        ReaderFirestoreProvider.chapterRepository.ofIdentifiers,
      ).mockReturnValue(createSuccessAsyncResult(allChapters));

      const { findPublishedChaptersByIdentifiers } = await import(
        "@/actions/chapter"
      );
      const result = await findPublishedChaptersByIdentifiers(identifiers);

      expect(result).toEqual([publishedChapter]);
      expect(result).not.toContainEqual(draftChapter);
    });

    it("全て下書きの場合は空配列を返す", async () => {
      const draftChapters = [
        Forger(ChapterMold).forge({ status: PublishStatus.DRAFT }),
        Forger(ChapterMold).forge({ status: PublishStatus.DRAFT }),
      ];
      const identifiers = draftChapters.map((chapter) => chapter.identifier);

      await setupUnwrapForAsyncResult();

      const { ReaderFirestoreProvider } = await import(
        "@/providers/infrastructure/firestore"
      );
      vi.mocked(
        ReaderFirestoreProvider.chapterRepository.ofIdentifiers,
      ).mockReturnValue(createSuccessAsyncResult(draftChapters));

      const { findPublishedChaptersByIdentifiers } = await import(
        "@/actions/chapter"
      );
      const result = await findPublishedChaptersByIdentifiers(identifiers);

      expect(result).toEqual([]);
    });

    it("空の識別子リストを渡すと空配列を返す", async () => {
      await setupUnwrapForAsyncResult();

      const { ReaderFirestoreProvider } = await import(
        "@/providers/infrastructure/firestore"
      );
      vi.mocked(
        ReaderFirestoreProvider.chapterRepository.ofIdentifiers,
      ).mockReturnValue(createSuccessAsyncResult([]));

      const { findPublishedChaptersByIdentifiers } = await import(
        "@/actions/chapter"
      );
      const result = await findPublishedChaptersByIdentifiers([]);

      expect(result).toEqual([]);
    });

    it("Chapter取得に失敗した場合はエラーがスローされる", async () => {
      const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(1);
      await setupUnwrapForAsyncResult();

      const { ReaderFirestoreProvider } = await import(
        "@/providers/infrastructure/firestore"
      );
      vi.mocked(
        ReaderFirestoreProvider.chapterRepository.ofIdentifiers,
      ).mockReturnValue(
        createErrorAsyncResult(
          aggregateNotFoundError("Chapter", "Chapter not found."),
        ),
      );

      const { findPublishedChaptersByIdentifiers } = await import(
        "@/actions/chapter"
      );

      await expect(
        findPublishedChaptersByIdentifiers([identifier]),
      ).rejects.toThrow();
    });
  });
});
