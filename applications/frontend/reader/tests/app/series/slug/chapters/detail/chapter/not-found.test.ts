/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
  Suspense: vi.fn(),
}));

vi.mock("next/navigation", () => ({
  notFound: vi.fn(() => {
    throw new Error("NEXT_NOT_FOUND");
  }),
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

vi.mock("@/actions/series", () => ({
  findBySlug: vi.fn(),
  searchAllChapterParams: vi.fn().mockResolvedValue([]),
}));

vi.mock("@/actions/chapter", () => ({
  findChapterBySlug: vi.fn(),
  findPublishedChaptersByIdentifiers: vi.fn().mockResolvedValue([]),
}));

vi.mock("@shared/components/molecules/skeleton", () => ({
  ArticleContentSkeleton: vi.fn().mockReturnValue(null),
}));

vi.mock("@shared/components/organisms/series/chapter/content", () => ({
  ChapterContent: vi.fn().mockReturnValue(null),
}));

vi.mock("@shared/components/global/mdx", () => ({
  MDXRenderer: vi.fn(),
}));

vi.mock("@shared/domains/common/slug", () => ({
  slugSchema: {
    parse: (slug: string) => slug,
  },
}));

vi.mock("@/providers/infrastructure/series", () => ({
  ReaderSeriesRepositoryProvider: {
    firebase: {
      find: vi.fn(),
      findBySlug: vi.fn(),
      search: vi.fn(),
    },
  },
}));

vi.mock("@/providers/infrastructure/chapter", () => ({
  ReaderChapterRepositoryProvider: {
    firebase: {
      find: vi.fn(),
      findBySlug: vi.fn(),
      ofIdentifiers: vi.fn(),
    },
  },
}));

describe("/series/[slug]/chapters/(detail)/[chapter] - not-found handling", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  it("存在しないseriesのslugでページを表示するとnotFound()がスローされる", async () => {
    const { findBySlug } = await import("@/actions/series");
    vi.mocked(findBySlug).mockRejectedValue(new Error("NEXT_NOT_FOUND"));

    const { default: ChapterPage } = await import(
      "@/app/series/[slug]/chapters/(detail)/[chapter]/page"
    );

    await expect(
      ChapterPage({
        params: Promise.resolve({
          slug: "nonexistent-series",
          chapter: "chapter-1",
        }),
      }),
    ).rejects.toThrow("NEXT_NOT_FOUND");
  });

  it("存在しないchapterのslugでページを表示するとnotFound()がスローされる", async () => {
    const { findBySlug } = await import("@/actions/series");
    const { findChapterBySlug } = await import("@/actions/chapter");
    vi.mocked(findBySlug).mockResolvedValue({ chapters: [] } as never);
    vi.mocked(findChapterBySlug).mockRejectedValue(new Error("NEXT_NOT_FOUND"));

    const { default: ChapterPage } = await import(
      "@/app/series/[slug]/chapters/(detail)/[chapter]/page"
    );

    await expect(
      ChapterPage({
        params: Promise.resolve({
          slug: "existing-series",
          chapter: "nonexistent-chapter",
        }),
      }),
    ).rejects.toThrow("NEXT_NOT_FOUND");
  });
});
