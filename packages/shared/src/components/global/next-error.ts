import { notFound } from "next/navigation";
import { AsyncResult } from "@shared/aspects/result";
import {
  isAggregateNotFoundError,
  isDuplicationError,
  isPermissionDeniedError,
  isResourceExhaustedError,
  isServiceUnavailableError,
  isUnauthenticatedError,
  isUnexpectedError,
  isValidationError,
} from "@shared/aspects/error";

export class ValidationHttpError extends Error {
  readonly statusCode = 400;

  constructor(message: string, public readonly errors: unknown[]) {
    super(message);
    this.name = "ValidationHttpError";
  }
}

export class UnauthenticatedHttpError extends Error {
  readonly statusCode = 401;

  constructor(message: string) {
    super(message);
    this.name = "UnauthenticatedHttpError";
  }
}

export class ForbiddenHttpError extends Error {
  readonly statusCode = 403;

  constructor(message: string) {
    super(message);
    this.name = "ForbiddenHttpError";
  }
}

export class ConflictHttpError extends Error {
  readonly statusCode = 409;

  constructor(message: string, public readonly entityName: string) {
    super(message);
    this.name = "ConflictHttpError";
  }
}

export class RateLimitHttpError extends Error {
  readonly statusCode = 429;

  constructor(message: string) {
    super(message);
    this.name = "RateLimitHttpError";
  }
}

export class UnexpectedHttpError extends Error {
  readonly statusCode = 500;

  constructor(message: string, cause?: unknown) {
    super(message, { cause });
    this.name = "UnexpectedHttpError";
  }
}

export class ServiceUnavailableHttpError extends Error {
  readonly statusCode = 503;

  constructor(message: string, public readonly retryable: boolean) {
    super(message);
    this.name = "ServiceUnavailableHttpError";
  }
}

export const isValidationHttpError = (
  error: unknown
): error is ValidationHttpError => {
  return error instanceof ValidationHttpError;
};

export const isUnauthenticatedHttpError = (
  error: unknown
): error is UnauthenticatedHttpError => {
  return error instanceof UnauthenticatedHttpError;
};

export const isForbiddenHttpError = (
  error: unknown
): error is ForbiddenHttpError => {
  return error instanceof ForbiddenHttpError;
};

export const isConflictHttpError = (
  error: unknown
): error is ConflictHttpError => {
  return error instanceof ConflictHttpError;
};

export const isRateLimitHttpError = (
  error: unknown
): error is RateLimitHttpError => {
  return error instanceof RateLimitHttpError;
};

export const isUnexpectedHttpError = (
  error: unknown
): error is UnexpectedHttpError => {
  return error instanceof UnexpectedHttpError;
};

export const isServiceUnavailableHttpError = (
  error: unknown
): error is ServiceUnavailableHttpError => {
  return error instanceof ServiceUnavailableHttpError;
};

const isDevelopment = () =>
  process.env.NODE_ENV ? process.env.NODE_ENV === "development" : false;

export type ErrorDetail = {
  type: string;
  message: string;
  statusCode: number;
  cause?: unknown;
  timestamp: string;
};

const createErrorDetail = (
  type: string,
  message: string,
  statusCode: number,
  cause?: unknown
): ErrorDetail => ({
  type,
  message,
  statusCode,
  cause: isDevelopment() ? cause : undefined,
  timestamp: new Date().toISOString(),
});

export const getErrorDetail = (error: unknown): ErrorDetail | null => {
  if (!isDevelopment()) {
    return null;
  }

  if (isValidationHttpError(error)) {
    return createErrorDetail(
      "ValidationError",
      error.message,
      400,
      error.errors
    );
  }

  if (isUnauthenticatedHttpError(error)) {
    return createErrorDetail("UnauthenticatedError", error.message, 401);
  }

  if (isForbiddenHttpError(error)) {
    return createErrorDetail("PermissionDeniedError", error.message, 403);
  }

  if (isConflictHttpError(error)) {
    return createErrorDetail("DuplicationError", error.message, 409, {
      entityName: error.entityName,
    });
  }

  if (isRateLimitHttpError(error)) {
    return createErrorDetail("ResourceExhaustedError", error.message, 429);
  }

  if (isServiceUnavailableHttpError(error)) {
    return createErrorDetail("ServiceUnavailableError", error.message, 503, {
      retryable: error.retryable,
    });
  }

  if (isUnexpectedHttpError(error)) {
    return createErrorDetail(
      "UnexpectedError",
      error.message,
      500,
      error.cause
    );
  }

  return createErrorDetail("UnknownError", String(error), 500, error);
};

export async function unwrapForNextJs<T, E>(
  asyncResult: AsyncResult<T, E>
): Promise<T> {
  return asyncResult.match({
    ok: (value) => value,
    err: (error) => {
      if (isDevelopment()) {
        const errorDetail = Array.isArray(error)
          ? error.map(getErrorDetail)
          : getErrorDetail(error);
        console.error(
          "[unwrapForNextJs] Error caught:",
          JSON.stringify(errorDetail, null, 2)
        );
      }

      if (isAggregateNotFoundError(error)) {
        notFound();
      }

      if (Array.isArray(error)) {
        if (error.every(isAggregateNotFoundError)) {
          notFound();
        }

        if (error.some(isValidationError)) {
          const validationErrors = error.filter(isValidationError);
          throw new ValidationHttpError("Validation failed", validationErrors);
        }
      }

      if (isValidationError(error)) {
        throw new ValidationHttpError(
          "Validation failed",
          Array.isArray(error) ? error : [error]
        );
      }

      if (isUnauthenticatedError(error)) {
        throw new UnauthenticatedHttpError(error.message);
      }

      if (isPermissionDeniedError(error)) {
        throw new ForbiddenHttpError(error.message);
      }

      if (isDuplicationError(error)) {
        throw new ConflictHttpError(error.message, error.name);
      }

      if (isResourceExhaustedError(error)) {
        throw new RateLimitHttpError(error.message);
      }

      if (isServiceUnavailableError(error)) {
        throw new ServiceUnavailableHttpError(error.message, error.retryable);
      }

      if (isUnexpectedError(error)) {
        throw new UnexpectedHttpError(error.message, error.cause);
      }

      throw new UnexpectedHttpError(
        error instanceof Error ? error.message : "Unexpected error occurred",
        error
      );
    },
  });
}
