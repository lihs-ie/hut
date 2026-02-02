"use server";

import { randomUUID } from "crypto";
import { cookies } from "next/headers";
import { FirebaseProvider } from "@shared/providers/infrastructure/firebase";
import { SearchReferenceIdentifier } from "@shared/domains/search-token/reference";
import { FirestoreOperations } from "@shared/infrastructures/common";
import { Firestore } from "firebase/firestore";

export type PageViewCounter = {
  count: number;
  updatedAt: string;
};

export type PageViewDedup = {
  createdAt: string;
};

const viewSessionCookieName = "pv_session";
const adminSessionCookieName = "admin_session";
const viewSessionMaxAgeSeconds = 60 * 60 * 24 * 365;
const jstOffsetMs = 9 * 60 * 60 * 1000;

/**
 * Builds a YYYY-MM-DD key in JST.
 */
export const getJstDateKey = (date: Date): string =>
  new Date(date.getTime() + jstOffsetMs).toISOString().slice(0, 10);

export type IncrementViewCountCoreParams = {
  firestore: Firestore;
  operations: FirestoreOperations;
  identifier: SearchReferenceIdentifier;
  sessionKey: string;
  now: Date;
};

/**
 * Core function for incrementing view count.
 * This function contains the Firestore transaction logic and is testable without cookies.
 */
export async function incrementViewCountCore(
  params: IncrementViewCountCoreParams,
): Promise<void> {
  const { firestore, operations, identifier, sessionKey, now } = params;
  const dateKey = getJstDateKey(now);
  const dedupDocId = `${dateKey}:${sessionKey}`;

  await operations.runTransaction(firestore, async (transaction) => {
    const dedupDoc = operations.doc<PageViewDedup>(
      firestore,
      "page-view-dedup",
      identifier.type,
      identifier.content,
      dedupDocId,
    );
    const dedupSnapshot = await transaction.get(dedupDoc);

    if (dedupSnapshot.exists()) {
      return;
    }

    const counterDoc = operations.doc<PageViewCounter>(
      firestore,
      "page-view-counters",
      identifier.type,
      identifier.content,
      dateKey,
    );
    const counterSnapshot = await transaction.get(counterDoc);
    const counterData = counterSnapshot.data();
    const currentCount = counterData?.count ?? 0;

    transaction.set(
      counterDoc,
      {
        count: currentCount + 1,
        updatedAt: now.toISOString(),
      },
      { merge: true },
    );
    transaction.set(
      dedupDoc,
      {
        createdAt: now.toISOString(),
      },
      { merge: true },
    );
  });
}

export async function incrementViewCount(
  identifier: SearchReferenceIdentifier,
): Promise<void> {
  const cookieStore = await cookies();

  if (cookieStore.get(adminSessionCookieName)?.value) {
    return;
  }

  const existingSession = cookieStore.get(viewSessionCookieName)?.value;
  const sessionKey = existingSession ?? randomUUID();

  if (!existingSession) {
    cookieStore.set(viewSessionCookieName, sessionKey, {
      httpOnly: true,
      secure: process.env.NODE_ENV === "production",
      sameSite: "lax",
      maxAge: viewSessionMaxAgeSeconds,
      path: "/",
    });
  }

  const { instance: firestore, operations } = FirebaseProvider.firestore;

  await incrementViewCountCore({
    firestore,
    operations,
    identifier,
    sessionKey,
    now: new Date(),
  });
}
