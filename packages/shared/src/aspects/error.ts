import z from "zod";
import { err, ok, Result } from "./result";

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
  _tag: Symbol.for("ValidationError"),
  field,
  description,
});

export const flattenValidationErrors = (
  errors: ValidationError[]
): ValidationError => {
  if (errors.length === 0) {
    return validationError("", "");
  }

  return {
    _tag: Symbol.for("ValidationError"),
    field: errors.map((error) => error.field).join(", "),
    description: errors.map((error) => error.description).join(", "),
  };
};

export const isValidationError = isError<ValidationError>(
  Symbol.for("ValidationError")
);

export const validate = <
  B extends string,
  T extends Readonly<{
    [k: string]: unknown;
  }> &
    z.core.$brand<B>
>(
  validation: z.core.$ZodBranded<z.ZodTypeAny, B>,
  candidate: unknown
): Result<T, ValidationError[]> => {
  const result = validation.safeParse(candidate);

  if (!result.success) {
    return err(
      result.error.issues.map((issue) =>
        validationError(issue.path.join("."), issue.message)
      )
    );
  }

  return ok(result.data as T);
};

export type AggregateNotFoundError<N extends string> = DomainError<{
  name: N;
  message: string;
}>;

export const aggregateNotFoundError = <N extends string>(
  name: N,
  message: string
): AggregateNotFoundError<N> => ({
  _tag: Symbol.for("AggregateNotFoundError"),
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
  _tag: Symbol.for("DuplicationError"),
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
  _tag: Symbol.for("UnexpectedError"),
  message,
  cause,
});

export const isUnexpectedError = isError<UnexpectedError>(
  Symbol.for("UnexpectedError")
);

export type UnauthenticatedError = DomainError<{
  message: string;
}>;

export const unauthenticatedError = (
  message: string
): UnauthenticatedError => ({
  _tag: Symbol.for("UnauthenticatedError"),
  message,
});

export const isUnauthenticatedError = isError<UnauthenticatedError>(
  Symbol.for("UnauthenticatedError")
);

export type PermissionDeniedError = DomainError<{
  message: string;
}>;

export const permissionDeniedError = (
  message: string
): PermissionDeniedError => ({
  _tag: Symbol.for("PermissionDeniedError"),
  message,
});

export const isPermissionDeniedError = isError<PermissionDeniedError>(
  Symbol.for("PermissionDeniedError")
);

export type ResourceExhaustedError = DomainError<{
  message: string;
}>;

export const resourceExhaustedError = (
  message: string
): ResourceExhaustedError => ({
  _tag: Symbol.for("ResourceExhaustedError"),
  message,
});

export const isResourceExhaustedError = isError<ResourceExhaustedError>(
  Symbol.for("ResourceExhaustedError")
);

export type ServiceUnavailableError = DomainError<{
  message: string;
  retryable: boolean;
}>;

export const serviceUnavailableError = (
  message: string,
  retryable: boolean = true
): ServiceUnavailableError => ({
  _tag: Symbol.for("ServiceUnavailableError"),
  message,
  retryable,
});

export const isServiceUnavailableError = isError<ServiceUnavailableError>(
  Symbol.for("ServiceUnavailableError")
);
