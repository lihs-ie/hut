import z from "zod";

export type DomainError<T> = T & {
  readonly _tag: symbol;
};

const isError =
  <E>(tag: symbol) =>
  (value: unknown): value is E =>
    value !== null &&
    typeof value === "object" &&
    "_tag" in value &&
    (value as unknown as DomainError<unknown>)._tag === tag;

export type ValidationError = DomainError<{
  field: string;
  description: string;
}>;

export const validationError = (
  field: string,
  description: string
): ValidationError => ({
  _tag: Symbol("ValidationError"),
  field,
  description,
});

export const isValidationError = isError<ValidationError>(
  Symbol.for("ValidationError")
);

export const validationErrors = <T>(
  parser: z.ZodObject,
  candidate: T
): ValidationError[] => {
  const result = parser.safeParse(candidate);

  if (result.success) {
    return [];
  }

  return result.error.issues.map((issue) =>
    validationError(issue.path.join("."), issue.message)
  );
};

export type AggregateNotFoundError<N extends string> = DomainError<{
  name: N;
  message: string;
}>;

export const aggregateNotFoundError = <N extends string>(
  name: N,
  message: string
): AggregateNotFoundError<N> => ({
  _tag: Symbol("AggregateNotFoundError"),
  name,
  message,
});

export const isAggregateNotFoundError = isError<AggregateNotFoundError<string>>(
  Symbol.for("AggregateNotFoundError")
);

export type DuplicationError<N extends string> = DomainError<{
  name: N;
  message: string;
}>;

export const duplicationError = <N extends string>(
  name: N,
  message: string
): DuplicationError<N> => ({
  _tag: Symbol("DuplicationError"),
  name,
  message,
});

export const isDuplicationError = isError<DuplicationError<string>>(
  Symbol.for("DuplicationError")
);

export type UnexpectedError = DomainError<{
  message: string;
  cause?: unknown;
}>;

export const unexpectedError = (
  message: string,
  cause?: unknown
): UnexpectedError => ({
  _tag: Symbol("UnexpectedError"),
  message,
  cause,
});

export const isUnexpectedError = isError<UnexpectedError>(
  Symbol.for("UnexpectedError")
);

// 認証エラー（未認証）
export type UnauthenticatedError = DomainError<{
  message: string;
}>;

export const unauthenticatedError = (
  message: string
): UnauthenticatedError => ({
  _tag: Symbol("UnauthenticatedError"),
  message,
});

export const isUnauthenticatedError = isError<UnauthenticatedError>(
  Symbol.for("UnauthenticatedError")
);

// 権限エラー（認証済みだが権限なし）
export type PermissionDeniedError = DomainError<{
  message: string;
}>;

export const permissionDeniedError = (
  message: string
): PermissionDeniedError => ({
  _tag: Symbol("PermissionDeniedError"),
  message,
});

export const isPermissionDeniedError = isError<PermissionDeniedError>(
  Symbol.for("PermissionDeniedError")
);

// リソース枯渇エラー（レート制限など）
export type ResourceExhaustedError = DomainError<{
  message: string;
}>;

export const resourceExhaustedError = (
  message: string
): ResourceExhaustedError => ({
  _tag: Symbol("ResourceExhaustedError"),
  message,
});

export const isResourceExhaustedError = isError<ResourceExhaustedError>(
  Symbol.for("ResourceExhaustedError")
);

// サービス利用不可エラー（一時的な障害）
export type ServiceUnavailableError = DomainError<{
  message: string;
  retryable: boolean;
}>;

export const serviceUnavailableError = (
  message: string,
  retryable: boolean = true
): ServiceUnavailableError => ({
  _tag: Symbol("ServiceUnavailableError"),
  message,
  retryable,
});

export const isServiceUnavailableError = isError<ServiceUnavailableError>(
  Symbol.for("ServiceUnavailableError")
);
