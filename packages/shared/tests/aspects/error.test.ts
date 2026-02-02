import { describe, it, expect } from "vitest";
import z from "zod";
import {
  validationError,
  flattenValidationErrors,
  isValidationError,
  validate,
  aggregateNotFoundError,
  isAggregateNotFoundError,
  duplicationError,
  isDuplicationError,
  unexpectedError,
  isUnexpectedError,
  unauthenticatedError,
  isUnauthenticatedError,
  permissionDeniedError,
  isPermissionDeniedError,
  resourceExhaustedError,
  isResourceExhaustedError,
  serviceUnavailableError,
  isServiceUnavailableError,
} from "@shared/aspects/error";

describe("aspects/error", () => {
  describe("ValidationError", () => {
    describe("validationError", () => {
      it("ValidationErrorを作成できる", () => {
        const error = validationError("email", "無効なメールアドレス形式です");

        expect(error.field).toBe("email");
        expect(error.description).toBe("無効なメールアドレス形式です");
        expect(error._tag).toBeDefined();
      });

      it("空のフィールド名でも作成できる", () => {
        const error = validationError("", "エラーメッセージ");

        expect(error.field).toBe("");
        expect(error.description).toBe("エラーメッセージ");
      });
    });

    describe("flattenValidationErrors", () => {
      it("複数のエラーを結合できる", () => {
        const errors = [
          validationError("email", "無効なメールアドレス"),
          validationError("password", "パスワードが短すぎます"),
        ];

        const flattened = flattenValidationErrors(errors);

        expect(flattened.field).toBe("email, password");
        expect(flattened.description).toBe(
          "無効なメールアドレス, パスワードが短すぎます"
        );
      });

      it("空の配列の場合は空のエラーを返す", () => {
        const flattened = flattenValidationErrors([]);

        expect(flattened.field).toBe("");
        expect(flattened.description).toBe("");
      });

      it("単一のエラーの場合はそのまま返す", () => {
        const errors = [validationError("name", "名前は必須です")];

        const flattened = flattenValidationErrors(errors);

        expect(flattened.field).toBe("name");
        expect(flattened.description).toBe("名前は必須です");
      });
    });

    describe("isValidationError", () => {
      it("ValidationErrorを正しく判定する", () => {
        const error = validationError("field", "message");

        expect(isValidationError(error)).toBe(true);
      });

      it("nullはfalseを返す", () => {
        expect(isValidationError(null)).toBe(false);
      });

      it("undefinedはfalseを返す", () => {
        expect(isValidationError(undefined)).toBe(false);
      });

      it("空のオブジェクトはfalseを返す", () => {
        expect(isValidationError({})).toBe(false);
      });

      it("異なる_tagを持つオブジェクトはfalseを返す", () => {
        expect(isValidationError({ _tag: Symbol("Other") })).toBe(false);
      });
    });
  });

  describe("validate", () => {
    const testSchema = z
      .object({
        name: z.string().min(1),
        age: z.number().positive(),
      })
      .brand("TestType");

    it("有効なデータでOkを返す", () => {
      const result = validate(testSchema, { name: "John", age: 25 });

      expect(result.isOk).toBe(true);
      const data = result.unwrap();
      expect(data.name).toBe("John");
      expect(data.age).toBe(25);
    });

    it("無効なデータでErrを返す", () => {
      const result = validate(testSchema, { name: "", age: -1 });

      expect(result.isErr).toBe(true);
      const errors = result.unwrapError();
      expect(errors.length).toBeGreaterThan(0);
      expect(errors.every(isValidationError)).toBe(true);
    });

    it("部分的に無効なデータでもエラーを返す", () => {
      const result = validate(testSchema, { name: "John", age: -1 });

      expect(result.isErr).toBe(true);
      const errors = result.unwrapError();
      expect(errors.length).toBe(1);
    });
  });

  describe("AggregateNotFoundError", () => {
    describe("aggregateNotFoundError", () => {
      it("AggregateNotFoundErrorを作成できる", () => {
        const error = aggregateNotFoundError("Article", "記事が見つかりません");

        expect(error.name).toBe("Article");
        expect(error.message).toBe("記事が見つかりません");
        expect(error._tag).toBeDefined();
      });
    });

    describe("isAggregateNotFoundError", () => {
      it("AggregateNotFoundErrorを正しく判定する", () => {
        const error = aggregateNotFoundError("User", "ユーザーが見つかりません");

        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("nullはfalseを返す", () => {
        expect(isAggregateNotFoundError(null)).toBe(false);
      });

      it("undefinedはfalseを返す", () => {
        expect(isAggregateNotFoundError(undefined)).toBe(false);
      });

      it("空のオブジェクトはfalseを返す", () => {
        expect(isAggregateNotFoundError({})).toBe(false);
      });

      it("異なる_tagを持つオブジェクトはfalseを返す", () => {
        expect(isAggregateNotFoundError({ _tag: Symbol("Other") })).toBe(false);
      });
    });
  });

  describe("DuplicationError", () => {
    describe("duplicationError", () => {
      it("DuplicationErrorを作成できる", () => {
        const error = duplicationError("Article", "記事が既に存在します");

        expect(error.name).toBe("Article");
        expect(error.message).toBe("記事が既に存在します");
        expect(error._tag).toBeDefined();
      });
    });

    describe("isDuplicationError", () => {
      it("DuplicationErrorを正しく判定する", () => {
        const error = duplicationError("Tag", "タグが既に存在します");

        expect(isDuplicationError(error)).toBe(true);
      });

      it("nullはfalseを返す", () => {
        expect(isDuplicationError(null)).toBe(false);
      });

      it("undefinedはfalseを返す", () => {
        expect(isDuplicationError(undefined)).toBe(false);
      });

      it("空のオブジェクトはfalseを返す", () => {
        expect(isDuplicationError({})).toBe(false);
      });

      it("異なる_tagを持つオブジェクトはfalseを返す", () => {
        expect(isDuplicationError({ _tag: Symbol("Other") })).toBe(false);
      });
    });
  });

  describe("UnexpectedError", () => {
    describe("unexpectedError", () => {
      it("UnexpectedErrorを作成できる", () => {
        const error = unexpectedError("予期しないエラーが発生しました");

        expect(error.message).toBe("予期しないエラーが発生しました");
        expect(error.cause).toBeUndefined();
        expect(error._tag).toBeDefined();
      });

      it("causeを指定してUnexpectedErrorを作成できる", () => {
        const cause = new Error("Original error");
        const error = unexpectedError("予期しないエラーが発生しました", cause);

        expect(error.message).toBe("予期しないエラーが発生しました");
        expect(error.cause).toBe(cause);
      });
    });

    describe("isUnexpectedError", () => {
      it("UnexpectedErrorを正しく判定する", () => {
        const error = unexpectedError("エラーメッセージ");

        expect(isUnexpectedError(error)).toBe(true);
      });

      it("nullはfalseを返す", () => {
        expect(isUnexpectedError(null)).toBe(false);
      });

      it("undefinedはfalseを返す", () => {
        expect(isUnexpectedError(undefined)).toBe(false);
      });

      it("空のオブジェクトはfalseを返す", () => {
        expect(isUnexpectedError({})).toBe(false);
      });

      it("異なる_tagを持つオブジェクトはfalseを返す", () => {
        expect(isUnexpectedError({ _tag: Symbol("Other") })).toBe(false);
      });
    });
  });

  describe("UnauthenticatedError", () => {
    describe("unauthenticatedError", () => {
      it("UnauthenticatedErrorを作成できる", () => {
        const error = unauthenticatedError("認証が必要です");

        expect(error.message).toBe("認証が必要です");
        expect(error._tag).toBeDefined();
      });
    });

    describe("isUnauthenticatedError", () => {
      it("UnauthenticatedErrorを正しく判定する", () => {
        const error = unauthenticatedError("認証エラー");

        expect(isUnauthenticatedError(error)).toBe(true);
      });

      it("nullはfalseを返す", () => {
        expect(isUnauthenticatedError(null)).toBe(false);
      });

      it("undefinedはfalseを返す", () => {
        expect(isUnauthenticatedError(undefined)).toBe(false);
      });

      it("空のオブジェクトはfalseを返す", () => {
        expect(isUnauthenticatedError({})).toBe(false);
      });

      it("異なる_tagを持つオブジェクトはfalseを返す", () => {
        expect(isUnauthenticatedError({ _tag: Symbol("Other") })).toBe(false);
      });
    });
  });

  describe("PermissionDeniedError", () => {
    describe("permissionDeniedError", () => {
      it("PermissionDeniedErrorを作成できる", () => {
        const error = permissionDeniedError("権限がありません");

        expect(error.message).toBe("権限がありません");
        expect(error._tag).toBeDefined();
      });
    });

    describe("isPermissionDeniedError", () => {
      it("PermissionDeniedErrorを正しく判定する", () => {
        const error = permissionDeniedError("アクセス拒否");

        expect(isPermissionDeniedError(error)).toBe(true);
      });

      it("nullはfalseを返す", () => {
        expect(isPermissionDeniedError(null)).toBe(false);
      });

      it("undefinedはfalseを返す", () => {
        expect(isPermissionDeniedError(undefined)).toBe(false);
      });

      it("空のオブジェクトはfalseを返す", () => {
        expect(isPermissionDeniedError({})).toBe(false);
      });

      it("異なる_tagを持つオブジェクトはfalseを返す", () => {
        expect(isPermissionDeniedError({ _tag: Symbol("Other") })).toBe(false);
      });
    });
  });

  describe("ResourceExhaustedError", () => {
    describe("resourceExhaustedError", () => {
      it("ResourceExhaustedErrorを作成できる", () => {
        const error = resourceExhaustedError("リソースが枯渇しました");

        expect(error.message).toBe("リソースが枯渇しました");
        expect(error._tag).toBeDefined();
      });
    });

    describe("isResourceExhaustedError", () => {
      it("ResourceExhaustedErrorを正しく判定する", () => {
        const error = resourceExhaustedError("制限超過");

        expect(isResourceExhaustedError(error)).toBe(true);
      });

      it("nullはfalseを返す", () => {
        expect(isResourceExhaustedError(null)).toBe(false);
      });

      it("undefinedはfalseを返す", () => {
        expect(isResourceExhaustedError(undefined)).toBe(false);
      });

      it("空のオブジェクトはfalseを返す", () => {
        expect(isResourceExhaustedError({})).toBe(false);
      });

      it("異なる_tagを持つオブジェクトはfalseを返す", () => {
        expect(isResourceExhaustedError({ _tag: Symbol("Other") })).toBe(false);
      });
    });
  });

  describe("ServiceUnavailableError", () => {
    describe("serviceUnavailableError", () => {
      it("ServiceUnavailableErrorを作成できる（デフォルトでretryable: true）", () => {
        const error = serviceUnavailableError("サービスが利用できません");

        expect(error.message).toBe("サービスが利用できません");
        expect(error.retryable).toBe(true);
        expect(error._tag).toBeDefined();
      });

      it("retryableをfalseに指定してServiceUnavailableErrorを作成できる", () => {
        const error = serviceUnavailableError(
          "サービスが永続的に停止しています",
          false
        );

        expect(error.message).toBe("サービスが永続的に停止しています");
        expect(error.retryable).toBe(false);
      });
    });

    describe("isServiceUnavailableError", () => {
      it("ServiceUnavailableErrorを正しく判定する", () => {
        const error = serviceUnavailableError("サービス停止");

        expect(isServiceUnavailableError(error)).toBe(true);
      });

      it("nullはfalseを返す", () => {
        expect(isServiceUnavailableError(null)).toBe(false);
      });

      it("undefinedはfalseを返す", () => {
        expect(isServiceUnavailableError(undefined)).toBe(false);
      });

      it("空のオブジェクトはfalseを返す", () => {
        expect(isServiceUnavailableError({})).toBe(false);
      });

      it("異なる_tagを持つオブジェクトはfalseを返す", () => {
        expect(isServiceUnavailableError({ _tag: Symbol("Other") })).toBe(
          false
        );
      });
    });
  });

  describe("エラー型の区別", () => {
    it("各エラー型が相互に排他的であることを確認", () => {
      const validationErr = validationError("field", "message");
      const notFoundErr = aggregateNotFoundError("Test", "message");
      const duplicationErr = duplicationError("Test", "message");
      const unexpectedErr = unexpectedError("message");
      const unauthenticatedErr = unauthenticatedError("message");
      const permissionDeniedErr = permissionDeniedError("message");
      const resourceExhaustedErr = resourceExhaustedError("message");
      const serviceUnavailableErr = serviceUnavailableError("message");

      // ValidationError
      expect(isValidationError(validationErr)).toBe(true);
      expect(isAggregateNotFoundError(validationErr)).toBe(false);
      expect(isDuplicationError(validationErr)).toBe(false);
      expect(isUnexpectedError(validationErr)).toBe(false);
      expect(isUnauthenticatedError(validationErr)).toBe(false);
      expect(isPermissionDeniedError(validationErr)).toBe(false);
      expect(isResourceExhaustedError(validationErr)).toBe(false);
      expect(isServiceUnavailableError(validationErr)).toBe(false);

      // AggregateNotFoundError
      expect(isValidationError(notFoundErr)).toBe(false);
      expect(isAggregateNotFoundError(notFoundErr)).toBe(true);
      expect(isDuplicationError(notFoundErr)).toBe(false);

      // DuplicationError
      expect(isValidationError(duplicationErr)).toBe(false);
      expect(isAggregateNotFoundError(duplicationErr)).toBe(false);
      expect(isDuplicationError(duplicationErr)).toBe(true);

      // UnexpectedError
      expect(isUnexpectedError(unexpectedErr)).toBe(true);
      expect(isValidationError(unexpectedErr)).toBe(false);

      // UnauthenticatedError
      expect(isUnauthenticatedError(unauthenticatedErr)).toBe(true);
      expect(isUnexpectedError(unauthenticatedErr)).toBe(false);

      // PermissionDeniedError
      expect(isPermissionDeniedError(permissionDeniedErr)).toBe(true);
      expect(isUnauthenticatedError(permissionDeniedErr)).toBe(false);

      // ResourceExhaustedError
      expect(isResourceExhaustedError(resourceExhaustedErr)).toBe(true);
      expect(isPermissionDeniedError(resourceExhaustedErr)).toBe(false);

      // ServiceUnavailableError
      expect(isServiceUnavailableError(serviceUnavailableErr)).toBe(true);
      expect(isResourceExhaustedError(serviceUnavailableErr)).toBe(false);
    });
  });
});
