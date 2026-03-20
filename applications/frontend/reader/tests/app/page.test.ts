/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { revalidate } from "../../src/app/page";

vi.mock("@shared/pages/page", () => ({
  default: vi.fn(),
}));

describe("/ (top) page", () => {
  describe("revalidate", () => {
    it("revalidate が 60 でexportされている", () => {
      expect(revalidate).toBe(60);
    });
  });
});
