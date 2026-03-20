/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";

vi.mock("@shared/pages/privacy/page", () => ({
  default: vi.fn(),
}));

describe("/privacy page", () => {
  describe("revalidate", () => {
    it("revalidate が 3600 でexportされている", async () => {
      const pageModule = await import("@/app/privacy/page");

      expect(pageModule.revalidate).toBe(3600);
    });
  });
});
