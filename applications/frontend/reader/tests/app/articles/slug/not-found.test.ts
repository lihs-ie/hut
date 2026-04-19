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

vi.mock("@/actions/article", () => ({
  findBySlug: vi.fn(),
  createTableOfContents: vi.fn(),
  searchAllSlugs: vi.fn().mockResolvedValue([]),
}));

vi.mock("@shared/actions/tag", () => ({
  findAllTags: vi.fn(),
}));

vi.mock("@shared/components/global/mdx", () => ({
  MDXRenderer: vi.fn(),
  generateTOC: vi.fn(),
}));

vi.mock("@shared/components/templates/article", () => ({
  ArticleIndex: vi.fn().mockReturnValue(null),
}));

vi.mock("@shared/actions/view", () => ({
  incrementViewCount: vi.fn(),
}));

vi.mock("@/providers/workflows/article", () => ({
  ArticleWorkflowProvider: {
    findBySlug: vi.fn(),
    search: vi.fn(),
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

vi.mock("@shared/providers/infrastructure/firebase", () => ({
  FirebaseProvider: {
    firestore: {
      get instance() {
        return {} as never;
      },
      get operations() {
        return {} as never;
      },
    },
  },
}));

vi.mock("@shared/providers/infrastructure/firebase-rest", () => ({
  FirebaseRestProvider: {
    firestore: {
      get instance() {
        return {} as never;
      },
      get operations() {
        return {} as never;
      },
    },
  },
}));

describe("/articles/[slug] - not-found handling", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  it("存在しないslugでページを表示するとnotFound()がスローされる", async () => {
    const { findBySlug } = await import("@/actions/article");
    vi.mocked(findBySlug).mockRejectedValue(new Error("NEXT_NOT_FOUND"));

    const { default: ArticleDetailPage } = await import(
      "@/app/articles/[slug]/page"
    );

    await expect(
      ArticleDetailPage({ params: Promise.resolve({ slug: "nonexistent-slug" }) }),
    ).rejects.toThrow("NEXT_NOT_FOUND");
  });
});
