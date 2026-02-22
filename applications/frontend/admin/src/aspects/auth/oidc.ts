import {
  AsyncResult,
  Result,
  err,
  fromPromise,
  ok,
} from "@shared/aspects/result";
import {
  Auth,
  GoogleAuthProvider,
  User,
  getRedirectResult as getFirebaseRedirectResult,
  onAuthStateChanged as onFirebaseAuthStateChanged,
  signInWithPopup,
  signInWithRedirect,
  signOut,
  type Unsubscribe,
} from "firebase/auth";

export type OidcUser = {
  readonly uid: string;
  readonly email: string;
  readonly displayName: string | null;
  readonly photoURL: string | null;
};

export type OidcAuthConfig = {
  readonly allowedEmails: readonly string[];
};

export type UserCancelledError = {
  readonly _tag: "UserCancelledError";
  readonly message: string;
};

export type NetworkError = {
  readonly _tag: "NetworkError";
  readonly message: string;
};

export type InvalidCredentialError = {
  readonly _tag: "InvalidCredentialError";
  readonly message: string;
};

export type UserDisabledError = {
  readonly _tag: "UserDisabledError";
  readonly message: string;
};

export type ConfigurationError = {
  readonly _tag: "ConfigurationError";
  readonly message: string;
};

export type OidcPermissionDeniedError = {
  readonly _tag: "OidcPermissionDeniedError";
  readonly message: string;
};

export type OidcUnexpectedError = {
  readonly _tag: "OidcUnexpectedError";
  readonly message: string;
  readonly cause?: unknown;
};

export type OidcAuthError =
  | UserCancelledError
  | NetworkError
  | InvalidCredentialError
  | UserDisabledError
  | ConfigurationError
  | OidcPermissionDeniedError
  | OidcUnexpectedError;

export const userCancelledError = (message: string): UserCancelledError => ({
  _tag: "UserCancelledError",
  message,
});

export const networkError = (message: string): NetworkError => ({
  _tag: "NetworkError",
  message,
});

export const invalidCredentialError = (
  message: string,
): InvalidCredentialError => ({
  _tag: "InvalidCredentialError",
  message,
});

export const userDisabledError = (message: string): UserDisabledError => ({
  _tag: "UserDisabledError",
  message,
});

export const configurationError = (message: string): ConfigurationError => ({
  _tag: "ConfigurationError",
  message,
});

export const oidcPermissionDeniedError = (
  message: string,
): OidcPermissionDeniedError => ({
  _tag: "OidcPermissionDeniedError",
  message,
});

export const oidcUnexpectedError = (
  message: string,
  cause?: unknown,
): OidcUnexpectedError => ({
  _tag: "OidcUnexpectedError",
  message,
  cause,
});

const isTaggedError = <T extends { _tag: string }>(
  value: unknown,
  tag: string,
): value is T =>
  value !== null &&
  typeof value === "object" &&
  "_tag" in value &&
  (value as { _tag?: unknown })._tag === tag;

export const isUserCancelledError = (
  value: unknown,
): value is UserCancelledError =>
  isTaggedError<UserCancelledError>(value, "UserCancelledError");

export const isNetworkError = (value: unknown): value is NetworkError =>
  isTaggedError<NetworkError>(value, "NetworkError");

export const isInvalidCredentialError = (
  value: unknown,
): value is InvalidCredentialError =>
  isTaggedError<InvalidCredentialError>(value, "InvalidCredentialError");

export const isUserDisabledError = (
  value: unknown,
): value is UserDisabledError =>
  isTaggedError<UserDisabledError>(value, "UserDisabledError");

export const isConfigurationError = (
  value: unknown,
): value is ConfigurationError =>
  isTaggedError<ConfigurationError>(value, "ConfigurationError");

export const isOidcPermissionDeniedError = (
  value: unknown,
): value is OidcPermissionDeniedError =>
  isTaggedError<OidcPermissionDeniedError>(value, "OidcPermissionDeniedError");

export const isOidcUnexpectedError = (
  value: unknown,
): value is OidcUnexpectedError =>
  isTaggedError<OidcUnexpectedError>(value, "OidcUnexpectedError");

type NormalizedOidcAuthConfig = {
  readonly allowedEmails: readonly string[];
};

const normalizeEmail = (email: string): string => email.trim().toLowerCase();

const normalizeConfig = (
  config: OidcAuthConfig,
): Result<NormalizedOidcAuthConfig, ConfigurationError> => {
  const normalized = config.allowedEmails
    .map(normalizeEmail)
    .filter((email) => email.length > 0);

  return ok({ allowedEmails: normalized });
};

const isAllowedEmail = (
  email: string,
  config: NormalizedOidcAuthConfig,
): boolean => {
  if (config.allowedEmails.length === 0) {
    return true;
  }
  const normalized = normalizeEmail(email);
  return config.allowedEmails.includes(normalized);
};

const isGoogleProviderUser = (user: User): boolean =>
  user.providerData.some(
    (provider) => provider.providerId === GoogleAuthProvider.PROVIDER_ID,
  );

const mapUser = (
  user: User,
  config: NormalizedOidcAuthConfig,
): Result<OidcUser, OidcAuthError> => {
  if (!isGoogleProviderUser(user)) {
    return err(invalidCredentialError("Unsupported authentication provider"));
  }

  if (user.email === null) {
    return err(invalidCredentialError("Missing email in Firebase user"));
  }

  if (!isAllowedEmail(user.email, config)) {
    return err(oidcPermissionDeniedError("The Google account is not allowed"));
  }

  return ok({
    uid: user.uid,
    email: user.email,
    displayName: user.displayName,
    photoURL: user.photoURL,
  });
};

const isFirebaseError = (
  value: unknown,
): value is { readonly code: string; readonly message: string } =>
  value !== null &&
  typeof value === "object" &&
  "code" in value &&
  "message" in value &&
  typeof (value as { code?: unknown }).code === "string" &&
  typeof (value as { message?: unknown }).message === "string";

const mapFirebaseError = (code: string, message: string): OidcAuthError => {
  switch (code) {
    case "auth/redirect-cancelled-by-user":
    case "auth/cancelled-popup-request":
    case "auth/popup-closed-by-user":
      return userCancelledError(message);
    case "auth/network-request-failed":
      return networkError(message);
    case "auth/invalid-credential":
    case "auth/credential-already-in-use":
    case "auth/account-exists-with-different-credential":
    case "auth/invalid-user-token":
    case "auth/user-token-expired":
      return invalidCredentialError(message);
    case "auth/user-disabled":
      return userDisabledError(message);
    case "auth/operation-not-allowed":
    case "auth/unauthorized-domain":
    case "auth/invalid-oauth-client-id":
    case "auth/invalid-api-key":
    case "auth/auth-domain-config-required":
    case "auth/invalid-auth-event":
    case "auth/invalid-continue-uri":
    case "auth/argument-error":
    case "auth/invalid-emulator-scheme":
      return configurationError(message);
    default:
      return oidcUnexpectedError(message, { code });
  }
};

const isOidcAuthError = (value: unknown): value is OidcAuthError =>
  isUserCancelledError(value) ||
  isNetworkError(value) ||
  isInvalidCredentialError(value) ||
  isUserDisabledError(value) ||
  isConfigurationError(value) ||
  isOidcPermissionDeniedError(value) ||
  isOidcUnexpectedError(value);

const mapAuthError = (error: unknown): OidcAuthError => {
  if (isOidcAuthError(error)) {
    return error;
  }

  if (isFirebaseError(error)) {
    return mapFirebaseError(error.code, error.message);
  }

  const message = error instanceof Error ? error.message : "Unexpected error";
  return oidcUnexpectedError(message, error);
};

export interface OidcAuth {
  startRedirect: () => AsyncResult<void, OidcAuthError>;
  startPopup: () => AsyncResult<OidcUser, OidcAuthError>;
  getRedirectResult: () => AsyncResult<OidcUser | null, OidcAuthError>;
  getIdToken: () => AsyncResult<string, OidcAuthError>;
  signOut: () => AsyncResult<void, OidcAuthError>;
  getCurrentUser: () => AsyncResult<OidcUser | null, OidcAuthError>;
  onAuthStateChanged: (
    handler: (user: OidcUser | null) => void,
  ) => AsyncResult<Unsubscribe, OidcAuthError>;
}

export const createOIDCAuth = (
  auth: Auth,
  config: OidcAuthConfig,
): OidcAuth => {
  const configResult = normalizeConfig(config);
  const provider = new GoogleAuthProvider();

  const requireConfig = (): Result<
    NormalizedOidcAuthConfig,
    ConfigurationError
  > => configResult;

  const startRedirect = (): AsyncResult<void, OidcAuthError> => {
    const normalizedConfig = requireConfig();

    if (normalizedConfig.isErr) {
      return err(normalizedConfig.unwrapError()).toAsync();
    }

    return fromPromise(signInWithRedirect(auth, provider), mapAuthError);
  };

  const startPopup = (): AsyncResult<OidcUser, OidcAuthError> => {
    const normalizedConfig = requireConfig();

    if (normalizedConfig.isErr) {
      return err(normalizedConfig.unwrapError()).toAsync();
    }

    return fromPromise(
      (async () => {
        const result = await signInWithPopup(auth, provider);

        if (result.providerId !== GoogleAuthProvider.PROVIDER_ID) {
          await signOut(auth);
          throw invalidCredentialError(
            `Unsupported provider: ${result.providerId}`,
          );
        }

        const mapped = mapUser(result.user, normalizedConfig.unwrap());

        if (mapped.isErr) {
          await signOut(auth);
          throw mapped.unwrapError();
        }

        return mapped.unwrap();
      })(),
      mapAuthError,
    );
  };

  const getRedirectResult = (): AsyncResult<OidcUser | null, OidcAuthError> => {
    const normalizedConfig = requireConfig();

    if (normalizedConfig.isErr) {
      return err(normalizedConfig.unwrapError()).toAsync();
    }

    return fromPromise(
      (async () => {
        const result = await getFirebaseRedirectResult(auth);

        if (result === null) {
          return null;
        }

        if (result.providerId !== GoogleAuthProvider.PROVIDER_ID) {
          await signOut(auth);
          throw invalidCredentialError(
            `Unsupported provider: ${result.providerId}`,
          );
        }

        const mapped = mapUser(result.user, normalizedConfig.unwrap());

        if (mapped.isErr) {
          await signOut(auth);
          throw mapped.unwrapError();
        }

        return mapped.unwrap();
      })(),
      mapAuthError,
    );
  };

  const signOutUser = (): AsyncResult<void, OidcAuthError> =>
    fromPromise(signOut(auth), mapAuthError);

  const getCurrentUser = (): AsyncResult<OidcUser | null, OidcAuthError> => {
    const normalizedConfig = requireConfig();

    if (normalizedConfig.isErr) {
      return err(normalizedConfig.unwrapError()).toAsync();
    }

    return fromPromise(
      (async () => {
        const current = auth.currentUser;

        if (current === null) {
          return null;
        }

        const mapped = mapUser(current, normalizedConfig.unwrap());

        if (mapped.isErr) {
          await signOut(auth);
          throw mapped.unwrapError();
        }

        return mapped.unwrap();
      })(),
      mapAuthError,
    );
  };

  const onAuthStateChanged = (
    handler: (user: OidcUser | null) => void,
  ): AsyncResult<Unsubscribe, OidcAuthError> => {
    const normalizedConfig = requireConfig();

    if (normalizedConfig.isErr) {
      return err(normalizedConfig.unwrapError()).toAsync();
    }

    const unsubscribe = onFirebaseAuthStateChanged(auth, (user) => {
      if (user === null) {
        handler(null);
        return;
      }

      const mapped = mapUser(user, normalizedConfig.unwrap());

      if (mapped.isErr) {
        void signOut(auth);
        handler(null);
        return;
      }

      handler(mapped.unwrap());
    });

    return ok(unsubscribe).toAsync();
  };

  const getIdToken = (): AsyncResult<string, OidcAuthError> =>
    fromPromise(
      (async () => {
        const currentUser = auth.currentUser;

        if (currentUser === null) {
          throw invalidCredentialError("No authenticated user");
        }

        return await currentUser.getIdToken();
      })(),
      mapAuthError,
    );

  return {
    startRedirect,
    startPopup,
    getRedirectResult,
    getIdToken,
    signOut: signOutUser,
    getCurrentUser,
    onAuthStateChanged,
  };
};
