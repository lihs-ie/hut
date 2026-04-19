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

vi.mock("@/actions/memo", () => ({
  findBySlug: vi.fn(),
  searchAllSlugs: vi.fn().mockResolvedValue([]),
}));

vi.mock("@shared/actions/tag", () => ({
  findAllTags: vi.fn(),
}));

vi.mock("@shared/components/global/mdx", () => ({
  MDXRenderer: vi.fn(),
}));

vi.mock("@shared/components/templates/memo", () => ({
  MemoIndex: vi.fn().mockReturnValue(null),
}));

vi.mock("@shared/actions/view", () => ({
  incrementViewCount: vi.fn(),
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

vi.mock("@/providers/infrastructure/memo", () => ({
  ReaderMemoRepositoryProvider: {
    firebase: {
      find: vi.fn(),
      findBySlug: vi.fn(),
      search: vi.fn(),
    },
  },
}));

describe("/memos/[slug] - not-found handling", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  it("存在しないslugでページを表示するとnotFound()がスローされる", async () => {
    const { findBySlug } = await import("@/actions/memo");
    vi.mocked(findBySlug).mockRejectedValue(new Error("NEXT_NOT_FOUND"));

    const { default: MemoDetailPage } = await import(
      "@/app/memos/[slug]/page"
    );

    await expect(
      MemoDetailPage({ params: Promise.resolve({ slug: "nonexistent-memo" }) }),
    ).rejects.toThrow("NEXT_NOT_FOUND");
  });
});
