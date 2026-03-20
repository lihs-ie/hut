/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { revalidate } from "../../../src/app/memos/page";

vi.mock("@shared/pages/memos/page", () => ({
  default: vi.fn(),
}));

describe("/memos page", () => {
  describe("revalidate", () => {
    it("revalidate が 60 でexportされている", () => {
      expect(revalidate).toBe(60);
    });
  });
});
