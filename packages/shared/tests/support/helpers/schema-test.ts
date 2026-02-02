import { describe, it, expect } from "vitest";
import type { ZodType } from "zod";

export function describeIdentifierSchema(
  schemaName: string,
  schema: ZodType,
  forgeIdentifier: () => string,
  forgeMultipleIdentifiers: (count: number) => string[],
): void {
  describe(`有効な${schemaName}の検証`, () => {
    it("有効なULIDは検証を通過する", () => {
      const identifier = forgeIdentifier();
      const result = schema.safeParse(identifier);
      expect(result.success).toBe(true);
    });

    it("複数の有効なULIDが検証を通過する", () => {
      const identifiers = forgeMultipleIdentifiers(5);
      for (const identifier of identifiers) {
        const result = schema.safeParse(identifier);
        expect(result.success).toBe(true);
      }
    });
  });

  describe(`無効な${schemaName}の検証`, () => {
    it("空文字列は無効", () => {
      const result = schema.safeParse("");
      expect(result.success).toBe(false);
    });

    it("不正な形式のULIDは無効", () => {
      const result = schema.safeParse("invalid-ulid");
      expect(result.success).toBe(false);
    });

    it("nullは無効", () => {
      const result = schema.safeParse(null);
      expect(result.success).toBe(false);
    });

    it("undefinedは無効", () => {
      const result = schema.safeParse(undefined);
      expect(result.success).toBe(false);
    });
  });
}

export function describeStringLengthSchema(
  schemaName: string,
  schema: ZodType,
  minLength: number,
  maxLength: number,
  options?: {
    minLengthMessage?: string;
    maxLengthMessage?: string;
    allowEmpty?: boolean;
  },
): void {
  describe(`有効な${schemaName}の検証`, () => {
    if (options?.allowEmpty) {
      it("空文字列は有効", () => {
        const result = schema.safeParse("");
        expect(result.success).toBe(true);
      });
    }

    if (minLength > 0) {
      it(`${minLength}文字は有効`, () => {
        const result = schema.safeParse("a".repeat(minLength));
        expect(result.success).toBe(true);
      });
    }

    it(`${maxLength}文字は有効`, () => {
      const result = schema.safeParse("a".repeat(maxLength));
      expect(result.success).toBe(true);
    });
  });

  describe(`無効な${schemaName}の検証`, () => {
    if (!options?.allowEmpty && minLength > 0) {
      it("空文字列は無効", () => {
        const result = schema.safeParse("");
        expect(result.success).toBe(false);
        if (!result.success && options?.minLengthMessage) {
          expect(result.error.issues[0].message).toBe(options.minLengthMessage);
        }
      });
    }

    it(`${maxLength + 1}文字以上は無効`, () => {
      const result = schema.safeParse("a".repeat(maxLength + 1));
      expect(result.success).toBe(false);
      if (!result.success && options?.maxLengthMessage) {
        expect(result.error.issues[0].message).toBe(options.maxLengthMessage);
      }
    });
  });
}

export function describeValidateFunction<_T, E>(
  validateFunction: (input: unknown) => {
    isOk: boolean;
    isErr: boolean;
    unwrapError?: () => E;
  },
  validInput: () => unknown,
  invalidInput: () => unknown,
  errorFieldName?: string,
): void {
  it("有効な入力でokを返す", () => {
    const result = validateFunction(validInput());
    expect(result.isOk).toBe(true);
  });

  it("無効な入力でerrを返す", () => {
    const result = validateFunction(invalidInput());
    expect(result.isErr).toBe(true);
    if (result.isErr && errorFieldName && result.unwrapError) {
      const error = result.unwrapError();
      if (error && typeof error === "object" && "field" in error) {
        expect((error as { field: string }).field).toBe(errorFieldName);
      }
    }
  });
}

export function describeEnumSchema(
  schemaName: string,
  schema: ZodType,
  validValues: string[],
  constantsToCheck?: Record<string, string>,
): void {
  describe(`有効な${schemaName}の検証`, () => {
    for (const value of validValues) {
      it(`'${value}' は有効`, () => {
        const result = schema.safeParse(value);
        expect(result.success).toBe(true);
      });
    }

    if (constantsToCheck) {
      it("定数が正しい値を持つ", () => {
        for (const [key, value] of Object.entries(constantsToCheck)) {
          expect(constantsToCheck[key]).toBe(value);
        }
      });
    }
  });

  describe(`無効な${schemaName}の検証`, () => {
    it("無効な文字列は無効", () => {
      const result = schema.safeParse("invalid");
      expect(result.success).toBe(false);
    });

    it("空文字列は無効", () => {
      const result = schema.safeParse("");
      expect(result.success).toBe(false);
    });

    it("nullは無効", () => {
      const result = schema.safeParse(null);
      expect(result.success).toBe(false);
    });
  });
}

export function expectImmutable<T>(
  original: T,
  operation: (item: T) => T,
  propertyToCheck: keyof T,
): void {
  const originalValue = original[propertyToCheck];
  operation(original);
  expect(original[propertyToCheck]).toEqual(originalValue);
}

export function describeObjectSchemaValidation(
  schemaName: string,
  schema: ZodType,
  validObject: () => unknown,
): void {
  describe(`無効な${schemaName}の共通検証`, () => {
    it("nullは無効", () => {
      const result = schema.safeParse(null);
      expect(result.success).toBe(false);
    });

    it("undefinedは無効", () => {
      const result = schema.safeParse(undefined);
      expect(result.success).toBe(false);
    });

    it("有効なオブジェクトは検証を通過する", () => {
      const result = schema.safeParse(validObject());
      expect(result.success).toBe(true);
    });
  });
}

export function describeForgerValidation<T>(
  description: string,
  schema: ZodType,
  forgeItem: () => T,
  forgeMultiple?: (count: number) => T[],
): void {
  it(description, () => {
    const item = forgeItem();
    const result = schema.safeParse(item);
    expect(result.success).toBe(true);
  });

  if (forgeMultiple) {
    it(`複数の${description.replace("は有効", "")}を生成できる`, () => {
      const items = forgeMultiple(5);
      for (const item of items) {
        const result = schema.safeParse(item);
        expect(result.success).toBe(true);
      }
    });
  }
}

/**
 * AsyncResult型からエラーを抽出するヘルパー関数
 * ok の場合は null を返し、err の場合はエラーを返す
 */
export async function extractError<T, E>(
  asyncResult: {
    match: <R>(handlers: {
      ok: (value: T) => R | Promise<R>;
      err: (error: E) => R | Promise<R>;
    }) => Promise<R>;
  },
): Promise<E | null> {
  return asyncResult.match({
    ok: () => null,
    err: (error) => error,
  });
}
