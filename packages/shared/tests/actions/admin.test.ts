/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { AdminMold } from "../support/molds/domains/user/common";
import { aggregateNotFoundError } from "@shared/aspects/error";
import {
  createSuccessAsyncResult,
  createErrorAsyncResult,
  setupUnwrapForAsyncResult,
} from "../support/helpers";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

const mockFind = vi.fn();

vi.mock("@shared/providers/workflows/admin", () => ({
  AdminWorkflowProvider: {
    find: mockFind,
  },
}));

describe("admin actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockFind.mockReset();
  });

  describe("getProfile", () => {
    it("管理者のプロフィールを正常に取得できる", async () => {
      const admin = Forger(AdminMold).forgeWithSeed(1);
      mockFind.mockReturnValue(createSuccessAsyncResult(admin));
      await setupUnwrapForAsyncResult();

      const { getProfile } = await import("@shared/actions/admin");
      const result = await getProfile();

      expect(result).toEqual(admin.profile);
      expect(mockFind).toHaveBeenCalled();
    });

    it("管理者が見つからない場合はエラーがスローされる", async () => {
      mockFind.mockReturnValue(
        createErrorAsyncResult(aggregateNotFoundError("Admin", "Admin not found."))
      );
      await setupUnwrapForAsyncResult();

      const { getProfile } = await import("@shared/actions/admin");

      await expect(getProfile()).rejects.toThrow();
    });
  });
});
