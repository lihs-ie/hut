/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { revalidate } from "../../../src/app/privacy/page";

vi.mock("@shared/pages/privacy/page", () => ({
  default: vi.fn(),
}));

describe("/privacy page", () => {
  describe("revalidate", () => {
    it("revalidate が 3600 でexportされている", () => {
      expect(revalidate).toBe(3600);
    });
  });
});
