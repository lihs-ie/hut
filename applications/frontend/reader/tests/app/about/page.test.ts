/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { dynamic } from "../../../src/app/about/page";

vi.mock("@shared/pages/about/page", () => ({
  default: vi.fn(),
}));

describe("/about page", () => {
  describe("dynamic", () => {
    it("dynamic が 'force-static' でexportされている", () => {
      expect(dynamic).toBe("force-static");
    });
  });
});
