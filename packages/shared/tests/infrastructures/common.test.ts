import { describe, it, expect } from "vitest";
import {
  createVersion,
  mapFirestoreError,
} from "@shared/infrastructures/common";
import {
  isAggregateNotFoundError,
  isDuplicationError,
  isUnexpectedError,
} from "@shared/aspects/error";

describe("infrastructures/common", () => {
  describe("createVersion", () => {
    it("指定した値でVersionを作成できる", () => {
      const version = createVersion(1);

      expect(version.value).toBe(1);
    });

    it("incrementで新しいVersionを作成できる", () => {
      const version = createVersion(1);
      const incremented = version.increment();

      expect(incremented.value).toBe(2);
      expect(version.value).toBe(1);
    });

    it("複数回incrementできる", () => {
      const version = createVersion(1);
      const v2 = version.increment();
      const v3 = v2.increment();
      const v4 = v3.increment();

      expect(v4.value).toBe(4);
    });

    it("0から始めることができる", () => {
      const version = createVersion(0);

      expect(version.value).toBe(0);
      expect(version.increment().value).toBe(1);
    });
  });

  describe("mapFirestoreError", () => {
    const mapError = mapFirestoreError("TestAggregate");

    it("not-foundエラーをAggregateNotFoundErrorに変換する", () => {
      const firestoreError = { code: "not-found", message: "Document not found" };
      const result = mapError(firestoreError);

      expect(isAggregateNotFoundError(result)).toBe(true);
      if (isAggregateNotFoundError(result)) {
        expect(result.name).toBe("TestAggregate");
        expect(result.message).toBe("Document not found");
      }
    });

    it("already-existsエラーをDuplicationErrorに変換する", () => {
      const firestoreError = { code: "already-exists", message: "Document already exists" };
      const result = mapError(firestoreError);

      expect(isDuplicationError(result)).toBe(true);
      if (isDuplicationError(result)) {
        expect(result.name).toBe("TestAggregate");
        expect(result.message).toBe("Document already exists");
      }
    });

    it("unauthenticatedエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "unauthenticated", message: "User not authenticated" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
      if (isUnexpectedError(result)) {
        expect(result.message).toBe("User not authenticated");
      }
    });

    it("permission-deniedエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "permission-denied", message: "Permission denied" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("resource-exhaustedエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "resource-exhausted", message: "Resource exhausted" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("unavailableエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "unavailable", message: "Service unavailable" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("deadline-exceededエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "deadline-exceeded", message: "Deadline exceeded" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("abortedエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "aborted", message: "Operation aborted" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("failed-preconditionエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "failed-precondition", message: "Failed precondition" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("invalid-argumentエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "invalid-argument", message: "Invalid argument" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("out-of-rangeエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "out-of-range", message: "Out of range" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("internalエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "internal", message: "Internal error" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("data-lossエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "data-loss", message: "Data loss" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("cancelledエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "cancelled", message: "Operation cancelled" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("unimplementedエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "unimplemented", message: "Not implemented" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("unknownエラーをUnexpectedErrorに変換する", () => {
      const firestoreError = { code: "unknown", message: "Unknown error" };
      const result = mapError(firestoreError);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("codeがないエラーをUnexpectedErrorに変換する", () => {
      const error = { message: "Some error" };
      const result = mapError(error);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("messageがないエラーを文字列変換する", () => {
      const error = { code: "unknown" };
      const result = mapError(error);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("文字列エラーを処理できる", () => {
      const result = mapError("Simple error string");

      expect(isUnexpectedError(result)).toBe(true);
      if (isUnexpectedError(result)) {
        expect(result.message).toBe("Simple error string");
      }
    });

    it("nullエラーを処理できる", () => {
      const result = mapError(null);

      expect(isUnexpectedError(result)).toBe(true);
    });

    it("undefinedエラーを処理できる", () => {
      const result = mapError(undefined);

      expect(isUnexpectedError(result)).toBe(true);
    });
  });
});
