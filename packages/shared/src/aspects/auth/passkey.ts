import z from "zod";
import { AsyncResult, err, fromPromise, ok, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  UnexpectedError,
  unexpectedError,
  ValidationError,
  validationErrors,
} from "@shared/aspects/error";
import {
  Firestore,
  collection,
  doc,
  getDoc,
  getDocs,
  setDoc,
  updateDoc,
  deleteDoc,
  Timestamp,
} from "firebase/firestore";
import { ulid } from "ulid";

// =============================================================================
// Passkey（WebAuthn）関連の型定義
// =============================================================================

/**
 * Passkey認証器情報
 * WebAuthn登録時に生成され、認証時に使用
 */
export const passkeyAuthenticatorSchema = z
  .object({
    credentialId: z.string().min(1),
    credentialPublicKey: z.string().min(1), // Base64エンコード
    counter: z.number().int().min(0),
    transports: z.array(z.string()).optional(),
    createdAt: z.date(),
  })
  .brand("PasskeyAuthenticator");

export type PasskeyAuthenticator = z.infer<typeof passkeyAuthenticatorSchema>;

export type UnvalidatedPasskeyAuthenticator = {
  credentialId: string;
  credentialPublicKey: string;
  counter: number;
  transports?: string[];
  createdAt: Date;
};

export const validatePasskeyAuthenticator = (
  candidate: UnvalidatedPasskeyAuthenticator
): Result<PasskeyAuthenticator, ValidationError[]> => {
  const errors = validationErrors(passkeyAuthenticatorSchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(passkeyAuthenticatorSchema.parse(candidate));
};

// =============================================================================
// Passkey登録/認証オプション
// =============================================================================

/**
 * Passkey登録オプション（クライアントに渡す）
 */
export type PasskeyRegistrationOptions = {
  readonly challenge: string;
  readonly rpId: string;
  readonly rpName: string;
  readonly userId: string;
  readonly userName: string;
  readonly userDisplayName: string;
};

/**
 * Passkey認証オプション（クライアントに渡す）
 */
export type PasskeyAuthenticationOptions = {
  readonly challenge: string;
  readonly rpId: string;
  readonly allowCredentials: { id: string; type: "public-key" }[];
};

/**
 * Passkey登録レスポンス（クライアントから受け取る）
 */
export type PasskeyRegistrationResponse = {
  readonly credentialId: string;
  readonly clientDataJSON: string;
  readonly attestationObject: string;
  readonly transports?: string[];
};

/**
 * Passkey認証レスポンス（クライアントから受け取る）
 */
export type PasskeyAuthenticationResponse = {
  readonly credentialId: string;
  readonly clientDataJSON: string;
  readonly authenticatorData: string;
  readonly signature: string;
};

// =============================================================================
// Passkey認証エラー
// =============================================================================

export type AuthenticationFailedError = {
  readonly _tag: symbol;
  readonly reason: string;
};

export const authenticationFailedError = (
  reason: string
): AuthenticationFailedError => ({
  _tag: Symbol("AuthenticationFailedError"),
  reason,
});

export const isAuthenticationFailedError = (
  value: unknown
): value is AuthenticationFailedError =>
  value !== null &&
  typeof value === "object" &&
  "_tag" in value &&
  "reason" in value;

// =============================================================================
// Passkey認証設定
// =============================================================================

/**
 * Passkey認証設定
 * 環境変数または設定ファイルから読み込む
 */
export type PasskeyAuthConfig = {
  readonly userId: string;
  readonly userName: string;
  readonly userDisplayName: string;
  readonly rpId: string; // Relying Party ID (ドメイン名)
  readonly rpName: string; // Relying Party Name (サイト名)
  readonly origin: string; // オリジン (https://example.com)
};

// =============================================================================
// Passkeyストレージ（Firebase直接操作）
// =============================================================================

type PersistedAuthenticator = {
  credentialId: string;
  credentialPublicKey: string;
  counter: number;
  transports?: string[];
  createdAt: Timestamp;
};

const mapError = (
  error: unknown
): AggregateNotFoundError<"PasskeyAuthenticator"> | UnexpectedError => {
  if (
    error &&
    typeof error === "object" &&
    "_tag" in error &&
    "name" in error
  ) {
    return error as AggregateNotFoundError<"PasskeyAuthenticator">;
  }
  const message = error instanceof Error ? error.message : String(error);
  return unexpectedError(message, error);
};

/**
 * Passkeyストレージ操作
 */
export type PasskeyStorage = {
  persist: (
    authenticator: PasskeyAuthenticator
  ) => AsyncResult<void, UnexpectedError>;
  findByCredentialId: (
    credentialId: string
  ) => AsyncResult<
    PasskeyAuthenticator,
    AggregateNotFoundError<"PasskeyAuthenticator"> | UnexpectedError
  >;
  findAll: () => AsyncResult<PasskeyAuthenticator[], UnexpectedError>;
  updateCounter: (
    credentialId: string,
    newCounter: number
  ) => AsyncResult<void, UnexpectedError>;
  terminate: (credentialId: string) => AsyncResult<void, UnexpectedError>;
};

export const createPasskeyStorage = (firestore: Firestore): PasskeyStorage => {
  const passkeyCollection = collection(firestore, "passkey_authenticators");

  const persist = (
    authenticator: PasskeyAuthenticator
  ): AsyncResult<void, UnexpectedError> =>
    fromPromise(
      (async () => {
        const docRef = doc(passkeyCollection, authenticator.credentialId);
        const data: PersistedAuthenticator = {
          credentialId: authenticator.credentialId,
          credentialPublicKey: authenticator.credentialPublicKey,
          counter: authenticator.counter,
          transports: authenticator.transports,
          createdAt: Timestamp.fromDate(authenticator.createdAt),
        };
        await setDoc(docRef, data);
      })(),
      mapError
    );

  const findByCredentialId = (
    credentialId: string
  ): AsyncResult<
    PasskeyAuthenticator,
    AggregateNotFoundError<"PasskeyAuthenticator"> | UnexpectedError
  > =>
    fromPromise(
      (async () => {
        const docRef = doc(passkeyCollection, credentialId);
        const snapshot = await getDoc(docRef);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "PasskeyAuthenticator",
            `Authenticator ${credentialId} not found.`
          );
        }

        const data = snapshot.data() as PersistedAuthenticator;
        return validatePasskeyAuthenticator({
          credentialId: data.credentialId,
          credentialPublicKey: data.credentialPublicKey,
          counter: data.counter,
          transports: data.transports,
          createdAt: data.createdAt.toDate(),
        }).unwrap();
      })(),
      mapError
    );

  const findAll = (): AsyncResult<PasskeyAuthenticator[], UnexpectedError> =>
    fromPromise(
      (async () => {
        const snapshot = await getDocs(passkeyCollection);
        return snapshot.docs.map((docSnapshot) => {
          const data = docSnapshot.data() as PersistedAuthenticator;
          return validatePasskeyAuthenticator({
            credentialId: data.credentialId,
            credentialPublicKey: data.credentialPublicKey,
            counter: data.counter,
            transports: data.transports,
            createdAt: data.createdAt.toDate(),
          }).unwrap();
        });
      })(),
      mapError
    );

  const updateCounter = (
    credentialId: string,
    newCounter: number
  ): AsyncResult<void, UnexpectedError> =>
    fromPromise(
      (async () => {
        const docRef = doc(passkeyCollection, credentialId);
        await updateDoc(docRef, { counter: newCounter });
      })(),
      mapError
    );

  const terminate = (
    credentialId: string
  ): AsyncResult<void, UnexpectedError> =>
    fromPromise(
      (async () => {
        const docRef = doc(passkeyCollection, credentialId);
        await deleteDoc(docRef);
      })(),
      mapError
    );

  return { persist, findByCredentialId, findAll, updateCounter, terminate };
};

// =============================================================================
// Passkey認証操作（WebAuthn）
// =============================================================================

/**
 * Passkey認証操作
 * @simplewebauthn/server を使用
 */
export type PasskeyAuth = {
  generateRegistrationOptions: (
    config: PasskeyAuthConfig
  ) => AsyncResult<PasskeyRegistrationOptions, UnexpectedError>;
  verifyRegistration: (
    config: PasskeyAuthConfig,
    expectedChallenge: string,
    response: PasskeyRegistrationResponse
  ) => AsyncResult<PasskeyAuthenticator, AuthenticationFailedError>;
  generateAuthenticationOptions: (
    config: PasskeyAuthConfig,
    authenticators: PasskeyAuthenticator[]
  ) => AsyncResult<PasskeyAuthenticationOptions, UnexpectedError>;
  verifyAuthentication: (
    config: PasskeyAuthConfig,
    expectedChallenge: string,
    authenticator: PasskeyAuthenticator,
    response: PasskeyAuthenticationResponse
  ) => AsyncResult<
    { verified: boolean; newCounter: number },
    AuthenticationFailedError
  >;
};

/**
 * SimpleWebAuthn を使用したPasskey認証操作を作成
 * 実際の使用時は @simplewebauthn/server をインストールしてください：
 * pnpm add @simplewebauthn/server
 */
export const createPasskeyAuth = (): PasskeyAuth => {
  const generateRegistrationOptions = (
    config: PasskeyAuthConfig
  ): AsyncResult<PasskeyRegistrationOptions, UnexpectedError> =>
    fromPromise(
      (async () => {
        // 実際は @simplewebauthn/server の generateRegistrationOptions を使用
        const challenge = ulid();

        return {
          challenge,
          rpId: config.rpId,
          rpName: config.rpName,
          userId: config.userId,
          userName: config.userName,
          userDisplayName: config.userDisplayName,
        };
      })(),
      (error) =>
        unexpectedError(
          error instanceof Error ? error.message : "Failed to generate options",
          error
        )
    );

  const verifyRegistration = (
    _config: PasskeyAuthConfig,
    _expectedChallenge: string,
    response: PasskeyRegistrationResponse
  ): AsyncResult<PasskeyAuthenticator, AuthenticationFailedError> =>
    fromPromise(
      (async () => {
        // 実際は @simplewebauthn/server の verifyRegistrationResponse を使用
        const result = validatePasskeyAuthenticator({
          credentialId: response.credentialId,
          credentialPublicKey: "verified-public-key", // 実際は検証結果から取得
          counter: 0,
          transports: response.transports,
          createdAt: new Date(),
        });

        if (result.isErr) {
          throw authenticationFailedError("Invalid authenticator data");
        }

        return result.unwrap();
      })(),
      (error) => {
        if (isAuthenticationFailedError(error)) {
          return error;
        }
        return authenticationFailedError(
          error instanceof Error ? error.message : "Registration failed"
        );
      }
    );

  const generateAuthenticationOptions = (
    config: PasskeyAuthConfig,
    authenticators: PasskeyAuthenticator[]
  ): AsyncResult<PasskeyAuthenticationOptions, UnexpectedError> =>
    fromPromise(
      (async () => {
        const challenge = ulid();

        return {
          challenge,
          rpId: config.rpId,
          allowCredentials: authenticators.map((auth) => ({
            id: auth.credentialId,
            type: "public-key" as const,
          })),
        };
      })(),
      (error) =>
        unexpectedError(
          error instanceof Error ? error.message : "Failed to generate options",
          error
        )
    );

  const verifyAuthentication = (
    _config: PasskeyAuthConfig,
    _expectedChallenge: string,
    authenticator: PasskeyAuthenticator,
    _response: PasskeyAuthenticationResponse
  ): AsyncResult<
    { verified: boolean; newCounter: number },
    AuthenticationFailedError
  > =>
    fromPromise(
      (async () => {
        // 実際は @simplewebauthn/server の verifyAuthenticationResponse を使用
        return {
          verified: true,
          newCounter: authenticator.counter + 1,
        };
      })(),
      (error) =>
        authenticationFailedError(
          error instanceof Error ? error.message : "Authentication failed"
        )
    );

  return {
    generateRegistrationOptions,
    verifyRegistration,
    generateAuthenticationOptions,
    verifyAuthentication,
  };
};
