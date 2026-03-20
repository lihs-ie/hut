/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";

vi.mock("@shared/pages/about/page", () => ({
  default: vi.fn(),
}));

describe("/about page", () => {
  describe("dynamic", () => {
    it("dynamic が 'force-static' でexportされている", async () => {
      const pageModule = await import("@/app/about/page");

      expect(pageModule.dynamic).toBe("force-static");
    });
  });
});
