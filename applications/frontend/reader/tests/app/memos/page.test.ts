/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";

vi.mock("@shared/pages/memos/page", () => ({
  default: vi.fn(),
}));

describe("/memos page", () => {
  describe("revalidate", () => {
    it("revalidate が 60 でexportされている", async () => {
      const pageModule = await import("@/app/memos/page");

      expect(pageModule.revalidate).toBe(60);
    });
  });
});
