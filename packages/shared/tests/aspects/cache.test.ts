import { describe, it, expect } from "vitest";
import {
  restoreDateFromCache,
  restoreTimelineFromCache,
} from "@shared/aspects/cache";
import type { Timeline } from "@shared/domains/common/date";

describe("aspects/cache", () => {
  describe("restoreDateFromCache", () => {
    describe("Dateオブジェクトの復元", () => {
      it("Dateオブジェクトはそのまま返す", () => {
        const date = new Date("2024-01-01T00:00:00Z");

        const result = restoreDateFromCache(date);

        expect(result).toBeInstanceOf(Date);
        expect(result).toBe(date);
      });
    });

    describe("ISO文字列からの復元", () => {
      it("ISO 8601形式の文字列をDateに変換", () => {
        const isoString = "2024-01-01T00:00:00.000Z";

        const result = restoreDateFromCache(isoString);

        expect(result).toBeInstanceOf(Date);
        expect(result.toISOString()).toBe(isoString);
      });

      it("タイムゾーン付きISO文字列をDateに変換", () => {
        const isoString = "2024-06-15T12:30:45.123Z";

        const result = restoreDateFromCache(isoString);

        expect(result).toBeInstanceOf(Date);
        expect(result.toISOString()).toBe(isoString);
      });

      it("unstable_cacheからのシリアライズされた日付文字列を変換", () => {
        // unstable_cacheはJSON.stringifyを使うため、Dateは ISO文字列になる
        const serialized = JSON.stringify({
          date: new Date("2024-01-15T10:00:00Z"),
        });
        const parsed = JSON.parse(serialized);

        const result = restoreDateFromCache(parsed.date);

        expect(result).toBeInstanceOf(Date);
        expect(result.toISOString()).toBe("2024-01-15T10:00:00.000Z");
      });
    });

    describe("境界ケース", () => {
      it("Unix epochの境界でも正しく変換", () => {
        const epochString = "1970-01-01T00:00:00.000Z";

        const result = restoreDateFromCache(epochString);

        expect(result).toBeInstanceOf(Date);
        expect(result.getTime()).toBe(0);
      });

      it("非常に古い日付も正しく変換", () => {
        const oldDateString = "1900-01-01T00:00:00.000Z";

        const result = restoreDateFromCache(oldDateString);

        expect(result).toBeInstanceOf(Date);
        expect(result.getFullYear()).toBe(1900);
      });

      it("未来の日付も正しく変換", () => {
        const futureDateString = "2100-12-31T23:59:59.999Z";

        const result = restoreDateFromCache(futureDateString);

        expect(result).toBeInstanceOf(Date);
        expect(result.toISOString()).toBe(futureDateString);
      });
    });
  });

  describe("restoreTimelineFromCache", () => {
    describe("Dateオブジェクトの復元", () => {
      it("DateオブジェクトのtimelineはそのままTimelineとして返す", () => {
        const createdAt = new Date("2024-01-01T00:00:00Z");
        const updatedAt = new Date("2024-01-02T00:00:00Z");

        const result = restoreTimelineFromCache({ createdAt, updatedAt });

        expect(result.createdAt).toBeInstanceOf(Date);
        expect(result.updatedAt).toBeInstanceOf(Date);
        expect(result.createdAt).toBe(createdAt);
        expect(result.updatedAt).toBe(updatedAt);
      });
    });

    describe("ISO文字列からの復元", () => {
      it("ISO文字列のtimelineをDateに変換", () => {
        const timeline = {
          createdAt: "2024-01-01T00:00:00.000Z",
          updatedAt: "2024-01-02T00:00:00.000Z",
        };

        const result = restoreTimelineFromCache(timeline);

        expect(result.createdAt).toBeInstanceOf(Date);
        expect(result.updatedAt).toBeInstanceOf(Date);
        expect(result.createdAt.toISOString()).toBe("2024-01-01T00:00:00.000Z");
        expect(result.updatedAt.toISOString()).toBe("2024-01-02T00:00:00.000Z");
      });

      it("unstable_cacheからのシリアライズされたtimelineを変換", () => {
        const original = {
          createdAt: new Date("2024-01-01T00:00:00Z"),
          updatedAt: new Date("2024-01-02T00:00:00Z"),
        };
        const serialized = JSON.stringify(original);
        const parsed = JSON.parse(serialized);

        const result = restoreTimelineFromCache(parsed);

        expect(result.createdAt).toBeInstanceOf(Date);
        expect(result.updatedAt).toBeInstanceOf(Date);
      });

      it("混合型(DateとString)のtimelineを変換", () => {
        const timeline = {
          createdAt: new Date("2024-01-01T00:00:00Z"),
          updatedAt: "2024-01-02T00:00:00.000Z",
        };

        const result = restoreTimelineFromCache(timeline);

        expect(result.createdAt).toBeInstanceOf(Date);
        expect(result.updatedAt).toBeInstanceOf(Date);
      });
    });

    describe("Timeline型の整合性", () => {
      it("返り値はTimeline型と互換性がある", () => {
        const timeline = {
          createdAt: "2024-01-01T00:00:00.000Z",
          updatedAt: "2024-01-02T00:00:00.000Z",
        };

        const result: Timeline = restoreTimelineFromCache(timeline);

        expect(result.createdAt).toBeInstanceOf(Date);
        expect(result.updatedAt).toBeInstanceOf(Date);
      });
    });
  });
});
