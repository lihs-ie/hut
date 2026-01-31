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
  setDoc,
  deleteDoc,
  Timestamp,
} from "firebase/firestore";

// =============================================================================
// セッション関連の型定義
// =============================================================================

export const sessionIdSchema = z.string().min(1).brand("SessionId");

export type SessionId = z.infer<typeof sessionIdSchema>;

export const sessionSchema = z
  .object({
    id: sessionIdSchema,
    adminIdentifier: z.string().min(1),
    expiresAt: z.date(),
    createdAt: z.date(),
  })
  .brand("AdminSession");

export type AdminSession = z.infer<typeof sessionSchema>;

export type UnvalidatedSession = {
  id: string;
  adminIdentifier: string;
  expiresAt: Date;
  createdAt: Date;
};

export const validateSession = (
  candidate: UnvalidatedSession
): Result<AdminSession, ValidationError[]> => {
  const errors = validationErrors(sessionSchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(sessionSchema.parse(candidate));
};

export const isSessionExpired = (session: AdminSession): boolean => {
  return new Date() > session.expiresAt;
};

export const isSessionValid = (session: AdminSession): boolean => {
  return !isSessionExpired(session);
};

// =============================================================================
// セッション関連エラー
// =============================================================================

export type SessionExpiredError = {
  readonly _tag: symbol;
  readonly sessionId: SessionId;
};

export const sessionExpiredError = (
  sessionId: SessionId
): SessionExpiredError => ({
  _tag: Symbol("SessionExpiredError"),
  sessionId,
});

export const isSessionExpiredError = (
  value: unknown
): value is SessionExpiredError =>
  value !== null &&
  typeof value === "object" &&
  "_tag" in value &&
  "sessionId" in value;

// =============================================================================
// セッションストレージ（Firebase直接操作）
// =============================================================================

type PersistedSession = {
  id: string;
  adminIdentifier: string;
  expiresAt: Timestamp;
  createdAt: Timestamp;
};

const mapError = (
  error: unknown
): AggregateNotFoundError<"AdminSession"> | UnexpectedError => {
  if (
    error &&
    typeof error === "object" &&
    "_tag" in error &&
    "name" in error
  ) {
    return error as AggregateNotFoundError<"AdminSession">;
  }
  const message = error instanceof Error ? error.message : String(error);
  return unexpectedError(message, error);
};

/**
 * セッションストレージ操作
 */
export type SessionStorage = {
  persist: (session: AdminSession) => AsyncResult<void, UnexpectedError>;
  find: (
    sessionId: SessionId
  ) => AsyncResult<
    AdminSession,
    AggregateNotFoundError<"AdminSession"> | UnexpectedError
  >;
  terminate: (sessionId: SessionId) => AsyncResult<void, UnexpectedError>;
};

export const createSessionStorage = (firestore: Firestore): SessionStorage => {
  const sessionsCollection = collection(firestore, "sessions");

  const persist = (session: AdminSession): AsyncResult<void, UnexpectedError> =>
    fromPromise(
      (async () => {
        const docRef = doc(sessionsCollection, session.id);
        const data: PersistedSession = {
          id: session.id,
          adminIdentifier: session.adminIdentifier,
          expiresAt: Timestamp.fromDate(session.expiresAt),
          createdAt: Timestamp.fromDate(session.createdAt),
        };
        await setDoc(docRef, data);
      })(),
      mapError
    );

  const find = (
    sessionId: SessionId
  ): AsyncResult<
    AdminSession,
    AggregateNotFoundError<"AdminSession"> | UnexpectedError
  > =>
    fromPromise(
      (async () => {
        const docRef = doc(sessionsCollection, sessionId);
        const snapshot = await getDoc(docRef);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "AdminSession",
            `Session ${sessionId} not found.`
          );
        }

        const data = snapshot.data() as PersistedSession;
        return validateSession({
          id: data.id,
          adminIdentifier: data.adminIdentifier,
          expiresAt: data.expiresAt.toDate(),
          createdAt: data.createdAt.toDate(),
        }).unwrap();
      })(),
      mapError
    );

  const terminate = (
    sessionId: SessionId
  ): AsyncResult<void, UnexpectedError> =>
    fromPromise(
      (async () => {
        const docRef = doc(sessionsCollection, sessionId);
        await deleteDoc(docRef);
      })(),
      mapError
    );

  return { persist, find, terminate };
};
