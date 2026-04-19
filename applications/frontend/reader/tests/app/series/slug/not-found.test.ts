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
  searchAllSlugs: vi.fn().mockResolvedValue([]),
  searchAllChapterParams: vi.fn().mockResolvedValue([]),
}));

vi.mock("@/actions/chapter", () => ({
  findPublishedChaptersByIdentifiers: vi.fn().mockResolvedValue([]),
  findChapterBySlug: vi.fn(),
}));

vi.mock("@shared/actions/tag", () => ({
  findAllTags: vi.fn(),
}));

vi.mock("@shared/components/templates/series", () => ({
  SeriesIndex: vi.fn().mockReturnValue(null),
}));

vi.mock("@shared/domains/common/slug", () => ({
  slugSchema: {
    parse: (slug: string) => slug,
  },
}));

vi.mock("@/providers/infrastructure/tag", () => ({
  ReaderTagRepositoryProvider: {
    firebase: {
      find: vi.fn(),
      ofIdentifiers: vi.fn(),
      search: vi.fn(),
    },
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

describe("/series/[slug] - not-found handling", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  it("存在しないslugでページを表示するとnotFound()がスローされる", async () => {
    const { findBySlug } = await import("@/actions/series");
    vi.mocked(findBySlug).mockRejectedValue(new Error("NEXT_NOT_FOUND"));

    const { default: SeriesDetailPage } = await import(
      "@/app/series/[slug]/page"
    );

    await expect(
      SeriesDetailPage({ params: Promise.resolve({ slug: "nonexistent-series" }) }),
    ).rejects.toThrow("NEXT_NOT_FOUND");
  });
});
