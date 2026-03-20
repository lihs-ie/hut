/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { revalidate } from "../../../src/app/articles/page";

vi.mock("@shared/pages/articles/page", () => ({
  default: vi.fn(),
}));

describe("/articles page", () => {
  describe("revalidate", () => {
    it("revalidate が 60 でexportされている", () => {
      expect(revalidate).toBe(60);
    });
  });
});
