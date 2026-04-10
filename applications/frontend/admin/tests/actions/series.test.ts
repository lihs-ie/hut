/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok } from "@shared/aspects/result";

const mockRequireAdmin = vi.fn();
const mockRevalidateTag = vi.fn();
const mockUnwrapForNextJs = vi.fn();
const mockNotifyReaderRevalidation = vi.fn();
const mockEventBrokerPublish = vi.fn();
const mockSeriesWorkflowPersist = vi.fn();
const mockSeriesWorkflowTerminate = vi.fn();

vi.mock("@/aspects/auth-guard", () => ({
  requireAdmin: mockRequireAdmin,
}));

vi.mock("next/cache", () => ({
  revalidateTag: mockRevalidateTag,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: mockUnwrapForNextJs,
}));

vi.mock("@/providers/domain/event", () => ({
  EventBrokerProvider: {
    pubSub: {
      publish: mockEventBrokerPublish,
    },
  },
}));

vi.mock("@/lib/revalidation", () => ({
  notifyReaderRevalidation: mockNotifyReaderRevalidation,
}));

vi.mock("@/providers/workflows/series", () => ({
  AdminSeriesWorkflowProvider: {
    persist: mockSeriesWorkflowPersist,
    terminateWithChapters: mockSeriesWorkflowTerminate,
    findBySlug: vi.fn(),
    search: vi.fn(),
  },
}));

const createMockAsyncResult = (value: unknown) => ({
  andThen: vi.fn().mockReturnValue(ok(value).toAsync()),
});

describe("actions/series", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation(
      (asyncResult: Promise<unknown>) => asyncResult,
    );
    mockEventBrokerPublish.mockReturnValue(ok(undefined).toAsync());
    mockNotifyReaderRevalidation.mockResolvedValue(undefined);
  });

  describe("persist", () => {
    const unvalidated = {
      title: "テスト連載",
      slug: "test-series",
      subTitle: null,
      cover: null,
      status: "DRAFT",
      tags: [],
      chapters: [],
    };

    beforeEach(() => {
      mockSeriesWorkflowPersist.mockReturnValue(
        createMockAsyncResult(undefined),
      );
    });

    it("Admin のキャッシュを series タグで revalidateTag する", async () => {
      const { persist } = await import("@/actions/series");

      await persist(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("series", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { persist } = await import("@/actions/series");

      await persist(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["series"]),
      );
    });
  });

  describe("terminate", () => {
    const identifier = "01HWXYZ0000000000000000005";

    beforeEach(() => {
      mockSeriesWorkflowTerminate.mockReturnValue(
        createMockAsyncResult(undefined),
      );
    });

    it("Admin のキャッシュを series タグで revalidateTag する", async () => {
      const { terminate } = await import("@/actions/series");

      await terminate(identifier);

      expect(mockRevalidateTag).toHaveBeenCalledWith("series", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { terminate } = await import("@/actions/series");

      await terminate(identifier);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["series"]),
      );
    });
  });
});
