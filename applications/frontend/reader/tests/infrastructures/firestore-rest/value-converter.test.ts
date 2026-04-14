import { describe, it, expect } from "vitest";
import { firestoreFieldsToObject } from "@/infrastructures/firestore-rest/value-converter";

describe("infrastructures/firestore-rest/value-converter", () => {
  describe("firestoreFieldsToObject", () => {
    it("stringValue を文字列に変換する", () => {
      const fields = {
        title: { stringValue: "サンプル記事" },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ title: "サンプル記事" });
    });

    it("integerValue を数値に変換する", () => {
      const fields = {
        count: { integerValue: "42" },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ count: 42 });
    });

    it("doubleValue を数値に変換する", () => {
      const fields = {
        rating: { doubleValue: 3.14 },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ rating: 3.14 });
    });

    it("booleanValue をブール値に変換する", () => {
      const fields = {
        isActive: { booleanValue: true },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ isActive: true });
    });

    it("nullValue を null に変換する", () => {
      const fields = {
        optional: { nullValue: null },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ optional: null });
    });

    it("timestampValue を ISO 文字列として保持する", () => {
      const fields = {
        createdAt: { timestampValue: "2024-01-15T10:30:00Z" },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ createdAt: "2024-01-15T10:30:00Z" });
    });

    it("arrayValue を配列に変換する", () => {
      const fields = {
        tags: {
          arrayValue: {
            values: [
              { stringValue: "tag1" },
              { stringValue: "tag2" },
            ],
          },
        },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ tags: ["tag1", "tag2"] });
    });

    it("空の arrayValue を空配列に変換する", () => {
      const fields = {
        tags: { arrayValue: {} },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ tags: [] });
    });

    it("mapValue を入れ子オブジェクトに変換する", () => {
      const fields = {
        timeline: {
          mapValue: {
            fields: {
              createdAt: { timestampValue: "2024-01-15T10:30:00Z" },
              updatedAt: { timestampValue: "2024-02-01T12:00:00Z" },
            },
          },
        },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({
        timeline: {
          createdAt: "2024-01-15T10:30:00Z",
          updatedAt: "2024-02-01T12:00:00Z",
        },
      });
    });

    it("空の mapValue を空オブジェクトに変換する", () => {
      const fields = {
        metadata: { mapValue: {} },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({ metadata: {} });
    });

    it("複数のフィールドを含むドキュメントを変換する", () => {
      const fields = {
        identifier: { stringValue: "01HSXAMPLE00000000000000" },
        title: { stringValue: "タイトル" },
        status: { stringValue: "published" },
        tags: {
          arrayValue: {
            values: [{ stringValue: "typescript" }],
          },
        },
        timeline: {
          mapValue: {
            fields: {
              createdAt: { timestampValue: "2024-01-15T10:30:00Z" },
              updatedAt: { timestampValue: "2024-02-01T12:00:00Z" },
            },
          },
        },
      };

      const result = firestoreFieldsToObject(fields);

      expect(result).toEqual({
        identifier: "01HSXAMPLE00000000000000",
        title: "タイトル",
        status: "published",
        tags: ["typescript"],
        timeline: {
          createdAt: "2024-01-15T10:30:00Z",
          updatedAt: "2024-02-01T12:00:00Z",
        },
      });
    });

    it("空の fields を空オブジェクトに変換する", () => {
      const result = firestoreFieldsToObject({});

      expect(result).toEqual({});
    });

    it("undefined の fields を空オブジェクトに変換する", () => {
      const result = firestoreFieldsToObject(undefined);

      expect(result).toEqual({});
    });
  });
});
