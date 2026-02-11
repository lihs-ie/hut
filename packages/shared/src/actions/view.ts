"use server";

import { randomUUID } from "crypto";
import { after } from "next/server";
import { cookies, headers } from "next/headers";
import { SearchReferenceIdentifier } from "@shared/domains/search-token/reference";
import { PageViewWorkflowProvider } from "@shared/providers/workflows/analytics/page-view";
import { UniqueVisitorWorkflowProvider } from "@shared/providers/workflows/analytics/unique-visitor";

const viewSessionCookieName = "pv_session";
const adminSessionCookieName = "admin_session";
const viewSessionMaxAgeSeconds = 60 * 60 * 24 * 365;

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

  const headerStore = await headers();
  const referrer = headerStore.get("referer") ?? null;
  const userAgent = headerStore.get("user-agent") ?? null;
  const now = new Date();

  after(async () => {
    await Promise.all([
      PageViewWorkflowProvider.record({
        now,
        payload: {
          reference: identifier,
          sessionKey,
          referrer,
          userAgent,
        },
      }).tapError((_error) => {}),
      UniqueVisitorWorkflowProvider.record({
        now,
        payload: { sessionKey },
      }).tapError((_error) => {}),
    ]);
  });
}
