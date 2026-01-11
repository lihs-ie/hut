import {
  type Firestore,
  type QueryDocumentSnapshot,
  Timestamp,
} from "firebase-admin/firestore";
import { AsyncResult, err, fromPromise, ok } from "@shared/aspects/result";
import {
  resourceExhaustedError,
  type ResourceExhaustedError,
  unexpectedError,
  type UnexpectedError,
} from "@shared/aspects/error";

export type RateLimitConfig = {
  key: string;
  limit: number;
  windowMs: number;
};

type RateLimit = {
  count: number;
  windowStart: Date;
};

type PersistedRateLimit = {
  count: number;
  windowStart: Timestamp;
};

type RateLimitDecision = {
  allowed: boolean;
};

const collectionName = "admin-rate-limits";
const exhaustedMessage =
  "ログイン試行が多すぎます。しばらくしてから再試行してください。";

export const enforceRateLimit = (
  firestore: Firestore,
  config: RateLimitConfig,
): AsyncResult<void, ResourceExhaustedError | UnexpectedError> => {
  const collection = firestore
    .collection(collectionName)
    .withConverter<RateLimit, PersistedRateLimit>({
      toFirestore(rateLimit: RateLimit): PersistedRateLimit {
        return {
          count: rateLimit.count,
          windowStart: Timestamp.fromDate(rateLimit.windowStart),
        };
      },
      fromFirestore(
        snapshot: QueryDocumentSnapshot<PersistedRateLimit>,
      ): RateLimit {
        const data = snapshot.data();
        return {
          count: data.count as number,
          windowStart: data.windowStart.toDate(),
        };
      },
    });

  return fromPromise(
    firestore.runTransaction(async (transaction) => {
      const now = new Date();
      const docRef = collection.doc(config.key);
      const snapshot = await transaction.get(docRef);

      let currentCount = 0;
      let windowStart = now;

      if (snapshot.exists) {
        const data = snapshot.data();
        if (data) {
          const elapsedMs = now.getTime() - data.windowStart.getTime();
          if (elapsedMs < config.windowMs) {
            currentCount = data.count;
            windowStart = data.windowStart;
          }
        }
      }

      const nextCount = currentCount + 1;

      if (nextCount > config.limit) {
        return { allowed: false } satisfies RateLimitDecision;
      }

      transaction.set(
        docRef,
        {
          count: nextCount,
          windowStart,
        },
        { merge: true },
      );

      return { allowed: true } satisfies RateLimitDecision;
    }),
    (error) =>
      unexpectedError(
        error instanceof Error ? error.message : "Failed to apply rate limit",
        error,
      ),
  ).andThen((decision) => {
    if (!decision.allowed) {
      return err(resourceExhaustedError(exhaustedMessage)).toAsync();
    }

    return ok<void, ResourceExhaustedError | UnexpectedError>(
      undefined,
    ).toAsync();
  });
};
