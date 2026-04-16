import { describe, it, expect } from "vitest";
import { computePublishedAt, isPublished } from "@shared/domains/common";
import { PublishStatus } from "@shared/domains/common/status";

describe("domains/common/publish", () => {
  describe("computePublishedAt", () => {
    it("新規作成 + nextStatus=PUBLISHED の場合は now を返す", () => {
      const now = new Date("2025-01-01T00:00:00Z");
      const result = computePublishedAt({
        currentStatus: null,
        nextStatus: PublishStatus.PUBLISHED,
        currentPublishedAt: null,
        now,
      });

      expect(result).toEqual(now);
    });

    it("新規作成 + nextStatus=DRAFT の場合は null を返す", () => {
      const now = new Date("2025-01-01T00:00:00Z");
      const result = computePublishedAt({
        currentStatus: null,
        nextStatus: PublishStatus.DRAFT,
        currentPublishedAt: null,
        now,
      });

      expect(result).toBeNull();
    });

    it("新規作成 + nextStatus=ARCHIVED の場合は null を返す", () => {
      const now = new Date("2025-01-01T00:00:00Z");
      const result = computePublishedAt({
        currentStatus: null,
        nextStatus: PublishStatus.ARCHIVED,
        currentPublishedAt: null,
        now,
      });

      expect(result).toBeNull();
    });

    it("currentStatus=DRAFT + nextStatus=PUBLISHED + currentPublishedAt=null の場合は now を返す", () => {
      const now = new Date("2025-06-15T12:00:00Z");
      const result = computePublishedAt({
        currentStatus: PublishStatus.DRAFT,
        nextStatus: PublishStatus.PUBLISHED,
        currentPublishedAt: null,
        now,
      });

      expect(result).toEqual(now);
    });

    it("currentStatus=PUBLISHED + nextStatus=DRAFT + currentPublishedAt=既存日時 の場合は既存日時を保持する", () => {
      const now = new Date("2025-06-15T12:00:00Z");
      const existingPublishedAt = new Date("2025-01-01T00:00:00Z");
      const result = computePublishedAt({
        currentStatus: PublishStatus.PUBLISHED,
        nextStatus: PublishStatus.DRAFT,
        currentPublishedAt: existingPublishedAt,
        now,
      });

      expect(result).toEqual(existingPublishedAt);
    });

    it("currentStatus=PUBLISHED + nextStatus=PUBLISHED + currentPublishedAt=既存日時 の場合は既存日時を保持し now で上書きしない", () => {
      const now = new Date("2025-06-15T12:00:00Z");
      const existingPublishedAt = new Date("2025-01-01T00:00:00Z");
      const result = computePublishedAt({
        currentStatus: PublishStatus.PUBLISHED,
        nextStatus: PublishStatus.PUBLISHED,
        currentPublishedAt: existingPublishedAt,
        now,
      });

      expect(result).toEqual(existingPublishedAt);
      expect(result).not.toEqual(now);
    });

    it("currentStatus=DRAFT + nextStatus=DRAFT + currentPublishedAt=null の場合は null のまま", () => {
      const now = new Date("2025-06-15T12:00:00Z");
      const result = computePublishedAt({
        currentStatus: PublishStatus.DRAFT,
        nextStatus: PublishStatus.DRAFT,
        currentPublishedAt: null,
        now,
      });

      expect(result).toBeNull();
    });
  });

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
