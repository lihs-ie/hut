/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { PrivacyPolicyMold } from "../support/molds/domains/document/common";
import { aggregateNotFoundError } from "@shared/aspects/error";
import {
  setupUnwrapWithResolvedValue,
  setupUnwrapWithRejectedValue,
} from "../support/helpers";

vi.mock("next/cache", () => ({
  unstable_cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

vi.mock("@shared/providers/workflows/document", () => ({
  DocumentWorkflowProvider: {
    GetPrivacyPolicy: vi.fn(),
  },
}));

describe("document actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  describe("getPrivacyPolicy", () => {
    it("プライバシーポリシーを正常に取得できる", async () => {
      const privacyPolicy = Forger(PrivacyPolicyMold).forgeWithSeed(1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(privacyPolicy);

      const { getPrivacyPolicy } = await import("@shared/actions/document");
      const result = await getPrivacyPolicy();

      expect(result).toEqual(privacyPolicy);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("プライバシーポリシーが見つからない場合はエラーがスローされる", async () => {
      await setupUnwrapWithRejectedValue(
        aggregateNotFoundError("SiteDocument", "SiteDocument not found.")
      );

      const { getPrivacyPolicy } = await import("@shared/actions/document");

      await expect(getPrivacyPolicy()).rejects.toThrow();
    });
  });
});
