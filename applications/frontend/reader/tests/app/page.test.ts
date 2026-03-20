/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";

vi.mock("@shared/pages/page", () => ({
  default: vi.fn(),
}));

describe("/ (top) page", () => {
  describe("revalidate", () => {
    it("revalidate が 60 でexportされている", async () => {
      const pageModule = await import("@/app/page");

      expect(pageModule.revalidate).toBe(60);
    });
  });
});
