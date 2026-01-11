import { type Auth as AdminAuth } from "firebase-admin/auth";
import { AsyncResult, fromPromise } from "@shared/aspects/result";
import {
  configurationError,
  invalidCredentialError,
  OidcAuthError,
  oidcUnexpectedError,
  OidcUser,
  userDisabledError,
} from "@/aspects/auth/oidc";

type OIDCServerAdaptorConfig = {
  readonly allowedEmails: readonly string[];
  readonly sessionExpiresInSeconds?: number;
};

export interface OIDCServerAdaptor {
  verifyIdToken: (idToken: string) => AsyncResult<OidcUser, OidcAuthError>;
  verifySessionCookie: (
    sessionCookie: string,
  ) => AsyncResult<OidcUser, OidcAuthError>;
  createSessionCookie: (idToken: string) => AsyncResult<string, OidcAuthError>;
  revokeSession: (sessionCookie: string) => AsyncResult<void, OidcAuthError>;
}

const normalizeEmail = (email: string): string => email.trim().toLowerCase();

const isAllowedEmail = (
  email: string,
  allowedEmails: readonly string[],
): boolean => {
  const normalized = normalizeEmail(email);

  return allowedEmails.map(normalizeEmail).includes(normalized);
};

const mapAdminAuthError = (error: unknown): OidcAuthError => {
  if (
    error !== null &&
    typeof error === "object" &&
    "code" in error &&
    typeof (error as { code?: unknown }).code === "string"
  ) {
    const code = (error as { code: string }).code;
    const message =
      error instanceof Error ? error.message : "Authentication error";

    switch (code) {
      case "auth/invalid-id-token":
      case "auth/id-token-expired":
      case "auth/id-token-revoked":
      case "auth/invalid-session-cookie":
      case "auth/session-cookie-expired":
      case "auth/session-cookie-revoked":
      case "auth/invalid-argument":
      case "auth/argument-error":
      case "auth/invalid-uid":
      case "auth/user-not-found":
        return invalidCredentialError(message);
      case "auth/user-disabled":
        return userDisabledError(message);
      case "auth/invalid-credential":
      case "auth/invalid-app-credential":
      case "auth/invalid-service-account":
      case "auth/app-not-authorized":
      case "auth/project-not-found":
      case "auth/insufficient-permission":
        return configurationError(message);
      default:
        return oidcUnexpectedError(message, error);
    }
  }

  const message = error instanceof Error ? error.message : "Unexpected error";
  return oidcUnexpectedError(message, error);
};

export const FirebaseOIDCServerAdaptor = (
  adminAuth: AdminAuth,
  config: OIDCServerAdaptorConfig,
): OIDCServerAdaptor => {
  const sessionExpiresInMs =
    (config.sessionExpiresInSeconds ?? 60 * 60 * 24) * 1000;
  const allowedEmails = config.allowedEmails;

  const verifyIdToken = (
    idToken: string,
  ): AsyncResult<OidcUser, OidcAuthError> =>
    fromPromise(
      (async () => {
        const decodedToken = await adminAuth.verifyIdToken(idToken);

        if (!decodedToken.email) {
          throw invalidCredentialError("Missing email in token");
        }

        if (!isAllowedEmail(decodedToken.email, allowedEmails)) {
          throw invalidCredentialError("Email not allowed");
        }

        return {
          uid: decodedToken.uid,
          email: decodedToken.email,
          displayName: decodedToken.name ?? null,
          photoURL: decodedToken.picture ?? null,
        };
      })(),
      mapAdminAuthError,
    );

  const verifySessionCookie = (
    sessionCookie: string,
  ): AsyncResult<OidcUser, OidcAuthError> =>
    fromPromise(
      (async () => {
        const decodedClaims = await adminAuth.verifySessionCookie(
          sessionCookie,
          true,
        );

        if (!decodedClaims.email) {
          throw invalidCredentialError("Missing email in session");
        }

        if (!isAllowedEmail(decodedClaims.email, allowedEmails)) {
          throw invalidCredentialError("Email not allowed");
        }

        return {
          uid: decodedClaims.uid,
          email: decodedClaims.email,
          displayName: decodedClaims.name ?? null,
          photoURL: decodedClaims.picture ?? null,
        };
      })(),
      mapAdminAuthError,
    );

  const createSessionCookie = (
    idToken: string,
  ): AsyncResult<string, OidcAuthError> =>
    fromPromise(
      adminAuth.createSessionCookie(idToken, {
        expiresIn: sessionExpiresInMs,
      }),
      mapAdminAuthError,
    );

  const revokeSession = (
    sessionCookie: string,
  ): AsyncResult<void, OidcAuthError> =>
    fromPromise(
      (async () => {
        const decodedClaims =
          await adminAuth.verifySessionCookie(sessionCookie);
        await adminAuth.revokeRefreshTokens(decodedClaims.sub);
      })(),
      mapAdminAuthError,
    );

  return {
    verifyIdToken,
    verifySessionCookie,
    createSessionCookie,
    revokeSession,
  };
};
