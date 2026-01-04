import { Logger } from "@/aspects/logger";
import {
  AggregateNotFoundError,
  UnexpectedError,
  ValidationError,
} from "@/aspects/error";
import { AsyncResult, err, ok } from "@/aspects/result";
import { ulid } from "ulid";
import {
  AdminSession,
  SessionId,
  SessionStorage,
  SessionExpiredError,
  isSessionExpired,
  sessionExpiredError,
  validateSession,
} from "./session";
import {
  PasskeyAuthConfig,
  PasskeyAuthenticator,
  PasskeyAuthenticationOptions,
  PasskeyAuthenticationResponse,
  PasskeyAuth,
  PasskeyStorage,
  PasskeyRegistrationOptions,
  PasskeyRegistrationResponse,
  AuthenticationFailedError,
  authenticationFailedError,
} from "./passkey";

// =============================================================================
// 管理者情報（認証用）
// =============================================================================

/**
 * 認証で使用する管理者情報
 * ドメイン層のAdminに依存せず、認証に必要な情報のみを持つ
 */
export type AuthAdmin = {
  readonly identifier: string;
  readonly name: string;
};

// =============================================================================
// 認証成功結果
// =============================================================================

export type AuthenticationSuccess = {
  readonly _tag: "AuthenticationSuccess";
  readonly admin: AuthAdmin;
  readonly session: AdminSession;
};

export const authenticationSuccess = (
  admin: AuthAdmin,
  session: AdminSession
): AuthenticationSuccess => ({
  _tag: "AuthenticationSuccess",
  admin,
  session,
});

// =============================================================================
// 管理者認証設定（単一管理者）
// =============================================================================

export type AdminAuthConfig = {
  readonly admin: AuthAdmin;
  readonly passkey: PasskeyAuthConfig;
};

// =============================================================================
// 認証コンテキスト
// =============================================================================

export type AuthContext = {
  readonly sessionStorage: SessionStorage;
  readonly passkeyStorage: PasskeyStorage;
  readonly passkeyAuth: PasskeyAuth;
  readonly adminAuthConfig: AdminAuthConfig;
  readonly logger: Logger;
};

// =============================================================================
// コマンド型
// =============================================================================

type Command<T> = {
  readonly timestamp: Date;
  readonly payload: T;
};

// =============================================================================
// Passkey登録開始
// =============================================================================

type RegistrationStartCommand = Command<Record<string, never>>;

export type RegistrationStartError = UnexpectedError;

export type RegistrationStart = (
  command: RegistrationStartCommand
) => AsyncResult<
  { options: PasskeyRegistrationOptions; challenge: string },
  RegistrationStartError
>;

/**
 * Passkey登録開始
 */
export const registrationStart =
  (ctx: AuthContext): RegistrationStart =>
  () => {
    ctx.logger.info("RegistrationStart started");

    return ctx.passkeyAuth
      .generateRegistrationOptions(ctx.adminAuthConfig.passkey)
      .tap((options) => {
        ctx.logger.debug("Registration options generated", {
          rpId: options.rpId,
          userId: options.userId,
        });
      })
      .map((options) => ({
        options,
        challenge: options.challenge,
      }))
      .tap(() => {
        ctx.logger.info("RegistrationStart completed");
      })
      .tapError((error) => {
        ctx.logger.error("RegistrationStart failed", { error });
      });
  };

// =============================================================================
// Passkey登録完了
// =============================================================================

type RegistrationFinishCommand = Command<{
  expectedChallenge: string;
  response: PasskeyRegistrationResponse;
}>;

export type RegistrationFinishError =
  | AuthenticationFailedError
  | ValidationError[]
  | UnexpectedError;

export type RegistrationFinish = (
  command: RegistrationFinishCommand
) => AsyncResult<PasskeyAuthenticator, RegistrationFinishError>;

/**
 * Passkey登録完了
 */
export const registrationFinish =
  (ctx: AuthContext): RegistrationFinish =>
  (command: RegistrationFinishCommand) => {
    ctx.logger.info("RegistrationFinish started");

    return ctx.passkeyAuth
      .verifyRegistration(
        ctx.adminAuthConfig.passkey,
        command.payload.expectedChallenge,
        command.payload.response
      )
      .tap((authenticator) => {
        ctx.logger.debug("Registration verified", {
          credentialId: authenticator.credentialId,
        });
      })
      .tapError((error) => {
        ctx.logger.warn("Registration verification failed", {
          reason: error.reason,
        });
      })
      .andThen((authenticator) =>
        ctx.passkeyStorage
          .persist(authenticator)
          .tap(() => {
            ctx.logger.debug("Authenticator persisted", {
              credentialId: authenticator.credentialId,
            });
          })
          .map(() => authenticator)
      )
      .tap((authenticator) => {
        ctx.logger.info("RegistrationFinish completed", {
          credentialId: authenticator.credentialId,
        });
      })
      .tapError((error) => {
        ctx.logger.error("RegistrationFinish failed", { error });
      });
  };

// =============================================================================
// Passkey認証開始
// =============================================================================

type AuthenticationStartCommand = Command<Record<string, never>>;

export type AuthenticationStartError = UnexpectedError;

export type AuthenticationStart = (
  command: AuthenticationStartCommand
) => AsyncResult<
  { options: PasskeyAuthenticationOptions; challenge: string },
  AuthenticationStartError
>;

/**
 * Passkey認証開始
 */
export const authenticationStart =
  (ctx: AuthContext): AuthenticationStart =>
  () => {
    ctx.logger.info("AuthenticationStart started");

    return ctx.passkeyStorage
      .findAll()
      .tap((authenticators) => {
        ctx.logger.debug("Authenticators found", {
          count: authenticators.length,
        });
      })
      .andThen((authenticators) =>
        ctx.passkeyAuth.generateAuthenticationOptions(
          ctx.adminAuthConfig.passkey,
          authenticators
        )
      )
      .map((options) => ({
        options,
        challenge: options.challenge,
      }))
      .tap(() => {
        ctx.logger.info("AuthenticationStart completed");
      })
      .tapError((error) => {
        ctx.logger.error("AuthenticationStart failed", { error });
      });
  };

// =============================================================================
// Passkey認証完了（ログイン）
// =============================================================================

type AuthenticationFinishCommand = Command<{
  expectedChallenge: string;
  response: PasskeyAuthenticationResponse;
}>;

export type AuthenticationFinishError =
  | AuthenticationFailedError
  | AggregateNotFoundError<"PasskeyAuthenticator">
  | ValidationError[]
  | UnexpectedError;

export type AuthenticationFinish = (
  command: AuthenticationFinishCommand
) => AsyncResult<AuthenticationSuccess, AuthenticationFinishError>;

/**
 * Passkey認証完了（ログイン）
 */
export const authenticationFinish =
  (ctx: AuthContext): AuthenticationFinish =>
  (command: AuthenticationFinishCommand) => {
    ctx.logger.info("AuthenticationFinish started");

    return ctx.passkeyStorage
      .findByCredentialId(command.payload.response.credentialId)
      .tap((authenticator) => {
        ctx.logger.debug("Authenticator found", {
          credentialId: authenticator.credentialId,
        });
      })
      .tapError((error) => {
        ctx.logger.warn("Authenticator not found", { error });
      })
      .andThen((authenticator) =>
        ctx.passkeyAuth
          .verifyAuthentication(
            ctx.adminAuthConfig.passkey,
            command.payload.expectedChallenge,
            authenticator,
            command.payload.response
          )
          .tap(({ verified }) => {
            ctx.logger.debug("Authentication verified", { verified });
          })
          .tapError((error) => {
            ctx.logger.warn("Authentication verification failed", {
              reason: error.reason,
            });
          })
          .andThen(({ verified, newCounter }) => {
            if (!verified) {
              return err(
                authenticationFailedError("Authentication verification failed")
              );
            }
            return ok({ newCounter, admin: ctx.adminAuthConfig.admin });
          })
          .andThen(({ newCounter, admin }) =>
            ctx.passkeyStorage
              .updateCounter(authenticator.credentialId, newCounter)
              .map(() => admin)
          )
      )
      .andThen((admin) => {
        const now = new Date();
        const expiresAt = new Date(now.getTime() + 24 * 60 * 60 * 1000); // 24時間後

        const sessionResult = validateSession({
          id: ulid(),
          adminIdentifier: admin.identifier,
          expiresAt,
          createdAt: now,
        });

        return sessionResult
          .toAsync()
          .tapError((errors) => {
            ctx.logger.error("Session validation failed", { errors });
          })
          .andThen((session) =>
            ctx.sessionStorage
              .persist(session)
              .tap(() => {
                ctx.logger.debug("Session persisted", {
                  sessionId: session.id,
                });
              })
              .tapError((error) => {
                ctx.logger.error("Failed to persist session", { error });
              })
              .map(() => authenticationSuccess(admin, session))
          );
      })
      .tap((result) => {
        ctx.logger.info("AuthenticationFinish completed", {
          adminIdentifier: result.admin.identifier,
          sessionId: result.session.id,
        });
      })
      .tapError((error) => {
        ctx.logger.error("AuthenticationFinish failed", { error });
      });
  };

// =============================================================================
// ログアウト
// =============================================================================

type LogoutCommand = Command<{
  sessionId: string;
}>;

export type LogoutError =
  | ValidationError
  | AggregateNotFoundError<"AdminSession">
  | SessionExpiredError
  | UnexpectedError;

export type Logout = (command: LogoutCommand) => AsyncResult<void, LogoutError>;

/**
 * ログアウト
 */
export const logout =
  (ctx: AuthContext): Logout =>
  (command: LogoutCommand) => {
    ctx.logger.info("Logout started", {
      sessionId: command.payload.sessionId,
    });

    const sessionId = command.payload.sessionId as SessionId;

    return ctx.sessionStorage
      .find(sessionId)
      .tap((session) => {
        ctx.logger.debug("Session found", { sessionId: session.id });
      })
      .tapError((error) => {
        ctx.logger.warn("Session not found", { error });
      })
      .andThen((session: AdminSession) => {
        if (isSessionExpired(session)) {
          ctx.logger.warn("Session expired", { sessionId: session.id });
          return err(sessionExpiredError(session.id));
        }
        return ok(session);
      })
      .andThen((session: AdminSession) =>
        ctx.sessionStorage
          .terminate(session.id)
          .tap(() => {
            ctx.logger.debug("Session terminated", { sessionId: session.id });
          })
          .tapError((error) => {
            ctx.logger.error("Failed to terminate session", { error });
          })
      )
      .tap(() => {
        ctx.logger.info("Logout completed", { sessionId });
      })
      .tapError((error) => {
        ctx.logger.error("Logout failed", { error });
      });
  };

// =============================================================================
// セッション検証
// =============================================================================

type ValidateSessionCommand = Command<{
  sessionId: string;
}>;

export type ValidateSessionError =
  | AggregateNotFoundError<"AdminSession">
  | SessionExpiredError
  | UnexpectedError;

export type ValidateSession = (
  command: ValidateSessionCommand
) => AsyncResult<AuthAdmin, ValidateSessionError>;

/**
 * セッション検証
 */
export const validateSessionAuth =
  (ctx: AuthContext): ValidateSession =>
  (command: ValidateSessionCommand) => {
    ctx.logger.info("ValidateSession started", {
      sessionId: command.payload.sessionId,
    });

    const sessionId = command.payload.sessionId as SessionId;

    return ctx.sessionStorage
      .find(sessionId)
      .tap((session) => {
        ctx.logger.debug("Session found", { sessionId: session.id });
      })
      .tapError((error) => {
        ctx.logger.warn("Session not found", { error });
      })
      .andThen((session: AdminSession) => {
        if (isSessionExpired(session)) {
          ctx.logger.warn("Session expired", { sessionId: session.id });
          return err(sessionExpiredError(session.id));
        }
        return ok(session);
      })
      .map(() => ctx.adminAuthConfig.admin)
      .tap((admin) => {
        ctx.logger.info("ValidateSession completed", {
          adminIdentifier: admin.identifier,
        });
      })
      .tapError((error) => {
        ctx.logger.error("ValidateSession failed", { error });
      });
  };
