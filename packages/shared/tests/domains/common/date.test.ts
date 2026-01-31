import { describe, it, expect } from "vitest";
import { timelineSchema, type Timeline } from "@shared/domains/common/date";

describe("domains/common/date", () => {
  describe("timelineSchema", () => {
    describe("有効なTimelineの検証", () => {
      it("createdAtとupdatedAtが同じ場合は有効", () => {
        const now = new Date();
        const timeline: Timeline = {
          createdAt: now,
          updatedAt: now,
        };

        const result = timelineSchema.safeParse(timeline);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.createdAt).toEqual(now);
          expect(result.data.updatedAt).toEqual(now);
        }
      });

      it("updatedAtがcreatedAtより後の場合は有効", () => {
        const createdAt = new Date("2024-01-01T00:00:00Z");
        const updatedAt = new Date("2024-01-02T00:00:00Z");
        const timeline: Timeline = {
          createdAt,
          updatedAt,
        };

        const result = timelineSchema.safeParse(timeline);

        expect(result.success).toBe(true);
      });

      it("数ミリ秒の差でも有効", () => {
        const createdAt = new Date("2024-01-01T00:00:00.000Z");
        const updatedAt = new Date("2024-01-01T00:00:00.001Z");
        const timeline: Timeline = {
          createdAt,
          updatedAt,
        };

        const result = timelineSchema.safeParse(timeline);

        expect(result.success).toBe(true);
      });
    });

    describe("無効なTimelineの検証", () => {
      it("updatedAtがcreatedAtより前の場合は無効", () => {
        const createdAt = new Date("2024-01-02T00:00:00Z");
        const updatedAt = new Date("2024-01-01T00:00:00Z");
        const timeline = {
          createdAt,
          updatedAt,
        };

        const result = timelineSchema.safeParse(timeline);

        expect(result.success).toBe(false);
        if (!result.success) {
          expect(result.error.issues[0].message).toBe(
            "updatedAt must be greater than or equal to createdAt"
          );
        }
      });

      it("createdAtがnullの場合は無効", () => {
        const result = timelineSchema.safeParse({
          createdAt: null,
          updatedAt: new Date(),
        });

        expect(result.success).toBe(false);
      });

      it("updatedAtがnullの場合は無効", () => {
        const result = timelineSchema.safeParse({
          createdAt: new Date(),
          updatedAt: null,
        });

        expect(result.success).toBe(false);
      });

      it("createdAtが欠けている場合は無効", () => {
        const result = timelineSchema.safeParse({
          updatedAt: new Date(),
        });

        expect(result.success).toBe(false);
      });

      it("updatedAtが欠けている場合は無効", () => {
        const result = timelineSchema.safeParse({
          createdAt: new Date(),
        });

        expect(result.success).toBe(false);
      });

      it("createdAtが文字列の場合は無効", () => {
        const result = timelineSchema.safeParse({
          createdAt: "2024-01-01T00:00:00Z",
          updatedAt: new Date(),
        });

        expect(result.success).toBe(false);
      });

      it("updatedAtが文字列の場合は無効", () => {
        const result = timelineSchema.safeParse({
          createdAt: new Date(),
          updatedAt: "2024-01-02T00:00:00Z",
        });

        expect(result.success).toBe(false);
      });

      it("空のオブジェクトは無効", () => {
        const result = timelineSchema.safeParse({});

        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = timelineSchema.safeParse(null);

        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = timelineSchema.safeParse(undefined);

        expect(result.success).toBe(false);
      });
    });

    describe("境界ケース", () => {
      it("非常に古い日付でも有効", () => {
        const createdAt = new Date("1900-01-01T00:00:00Z");
        const updatedAt = new Date("1900-01-02T00:00:00Z");
        const timeline = {
          createdAt,
          updatedAt,
        };

        const result = timelineSchema.safeParse(timeline);

        expect(result.success).toBe(true);
      });

      it("未来の日付でも有効", () => {
        const createdAt = new Date("2100-01-01T00:00:00Z");
        const updatedAt = new Date("2100-01-02T00:00:00Z");
        const timeline = {
          createdAt,
          updatedAt,
        };

        const result = timelineSchema.safeParse(timeline);

        expect(result.success).toBe(true);
      });

      it("Unix epochの境界でも有効", () => {
        const createdAt = new Date(0); // 1970-01-01T00:00:00.000Z
        const updatedAt = new Date(1);
        const timeline = {
          createdAt,
          updatedAt,
        };

        const result = timelineSchema.safeParse(timeline);

        expect(result.success).toBe(true);
      });
    });
  });

  describe("Timeline型", () => {
    it("正しい型の構造を持つ", () => {
      const timeline: Timeline = {
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      expect(timeline.createdAt).toBeInstanceOf(Date);
      expect(timeline.updatedAt).toBeInstanceOf(Date);
    });
  });
});
