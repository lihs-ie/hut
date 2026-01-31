"use server";

import { cache } from "react";
import {
  OidcAuthError,
  OidcUser,
  invalidCredentialError,
  isConfigurationError,
  isInvalidCredentialError,
  isNetworkError,
  isOidcPermissionDeniedError,
  isUserCancelledError,
  isUserDisabledError,
  oidcUnexpectedError,
} from "@/aspects/auth/oidc";
import {
  clearSessionCookie,
  getSessionCookie,
  setSessionCookie,
} from "@/aspects/cookie";
import { OIDCServerProvider } from "@/providers/acl/oidc/server";
import {
  permissionDeniedError,
  PermissionDeniedError,
  serviceUnavailableError,
  ServiceUnavailableError,
  unauthenticatedError,
  UnauthenticatedError,
  unexpectedError,
  UnexpectedError,
} from "@shared/aspects/error";
import {
  AsyncResult,
  Result,
  err,
  fromPromise,
  ok,
} from "@shared/aspects/result";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { redirect } from "next/navigation";

const mapOidcErrorToShared = (
  error: OidcAuthError,
):
  | PermissionDeniedError
  | UnauthenticatedError
  | ServiceUnavailableError
  | UnexpectedError => {
  if (isOidcPermissionDeniedError(error)) {
    return permissionDeniedError(error.message);
  }

  if (isInvalidCredentialError(error) || isUserDisabledError(error)) {
    return unauthenticatedError(error.message);
  }

  if (isUserCancelledError(error)) {
    return unauthenticatedError(error.message);
  }

  if (isNetworkError(error)) {
    return serviceUnavailableError(error.message, true);
  }

  if (isConfigurationError(error)) {
    return unexpectedError(error.message, error);
  }

  return unexpectedError(error.message, error.cause);
};

const sessionCookieMaxAgeSeconds = 60 * 60 * 24;

const ensureIdToken = (idToken: string): Result<string, OidcAuthError> => {
  const trimmed = idToken.trim();

  if (trimmed.length === 0) {
    return err(invalidCredentialError("ID token is required"));
  }

  return ok(trimmed);
};

const recoverFromSessionError = (
  error: OidcAuthError,
): AsyncResult<OidcUser | null, OidcAuthError> => {
  if (
    isInvalidCredentialError(error) ||
    isUserDisabledError(error) ||
    isOidcPermissionDeniedError(error)
  ) {
    return ok<OidcUser | null, OidcAuthError>(null).toAsync();
  }

  return err(error).toAsync();
};

export async function login(idToken: string): Promise<void> {
  return await unwrapForNextJs(
    ensureIdToken(idToken)
      .toAsync()
      .andThen((token) =>
        OIDCServerProvider.verifyIdToken(token)
          .andThen(() => OIDCServerProvider.createSessionCookie(token))
          .andThen((sessionCookie) =>
            fromPromise(
              setSessionCookie(sessionCookie, {
                maxAge: sessionCookieMaxAgeSeconds,
              }),
              (error) =>
                oidcUnexpectedError(
                  error instanceof Error
                    ? error.message
                    : "Failed to set session",
                  error,
                ),
            ),
          ),
      )
      .mapError(mapOidcErrorToShared),
  );
}

export const getSession = cache(async (): Promise<OidcUser | null> => {
  return await unwrapForNextJs(
    fromPromise(getSessionCookie(), (error) =>
      oidcUnexpectedError(
        error instanceof Error
          ? error.message
          : "Failed to read session cookie",
        error,
      ),
    )
      .andThen((sessionCookie) => {
        if (sessionCookie === null) {
          return ok<OidcUser | null, OidcAuthError>(null).toAsync();
        }

        return OIDCServerProvider.verifySessionCookie(sessionCookie)
          .map((user): OidcUser | null => user)
          .orElse(recoverFromSessionError);
      })
      .mapError(mapOidcErrorToShared),
  );
});

export async function logout(): Promise<void> {
  await unwrapForNextJs(
    fromPromise(getSessionCookie(), (error) =>
      oidcUnexpectedError(
        error instanceof Error
          ? error.message
          : "Failed to read session cookie",
        error,
      ),
    )
      .andThen((sessionCookie) => {
        if (sessionCookie === null) {
          return ok<void, OidcAuthError>(undefined).toAsync();
        }

        return OIDCServerProvider.revokeSession(sessionCookie).orElse(() =>
          ok<void, OidcAuthError>(undefined).toAsync(),
        );
      })
      .andThen(() =>
        fromPromise(clearSessionCookie(), (error) =>
          oidcUnexpectedError(
            error instanceof Error ? error.message : "Failed to clear session",
            error,
          ),
        ),
      )
      .mapError(mapOidcErrorToShared),
  );

  redirect("/admin/login");
}

export async function isAdmin(): Promise<boolean> {
  const session = await getSession();

  return session !== null;
}
