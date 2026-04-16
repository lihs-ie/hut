import { describe, it, expect } from "vitest";
import { isPublished } from "@shared/domains/common";

describe("domains/common/publish", () => {
  describe("isPublished", () => {
    it("publishedAt が Date の場合は true を返す", () => {
      const entity = { publishedAt: new Date("2025-01-01T00:00:00Z") };
      expect(isPublished(entity)).toBe(true);
    });

    it("publishedAt が null の場合は false を返す", () => {
      const entity = { publishedAt: null };
      expect(isPublished(entity)).toBe(false);
    });

    it("publishedAt が undefined の場合は false を返す", () => {
      const entity = { publishedAt: undefined };
      expect(isPublished(entity)).toBe(false);
    });

    it("型ガードによりフィルタ後の配列要素で publishedAt が Date として推論される", () => {
      const entities = [
        { publishedAt: new Date("2025-01-01"), title: "published" },
        { publishedAt: null, title: "draft" },
        { publishedAt: undefined, title: "no-field" },
      ];

      const published = entities.filter(isPublished);

      expect(published).toHaveLength(1);
      expect(published[0].publishedAt.getFullYear()).toBe(2025);
      expect(published[0].title).toBe("published");
    });
  });
});
