/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";

vi.mock("@shared/pages/articles/page", () => ({
  default: vi.fn(),
}));

describe("/articles page", () => {
  describe("revalidate", () => {
    it("revalidate が 60 でexportされている", async () => {
      const pageModule = await import("@/app/articles/page");

      expect(pageModule.revalidate).toBe(60);
    });
  });
});
