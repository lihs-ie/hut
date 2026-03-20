/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { revalidate } from "../../../src/app/about/page";

vi.mock("@shared/pages/about/page", () => ({
  default: vi.fn(),
}));

describe("/about page", () => {
  describe("revalidate", () => {
    it("revalidate が 3600 でexportされている", () => {
      expect(revalidate).toBe(3600);
    });
  });
});
