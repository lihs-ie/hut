import { FirebaseAdminProvider } from "@/providers/auth/admin";
import { OIDCServerProvider } from "@/providers/acl/oidc/server";
import {
  isInvalidCredentialError,
  isOidcPermissionDeniedError,
  isUserDisabledError,
} from "@/aspects/auth/oidc";
import { isE2EAuthAvailable } from "@/aspects/e2e";
import { type Auth as AdminAuth } from "firebase-admin/auth";
import { NextRequest, NextResponse } from "next/server";

export const runtime = "nodejs";

type E2EAuthRequest = {
  readonly uid?: string;
  readonly email?: string;
};

type E2EAuthPayload = {
  readonly uid: string;
  readonly email: string;
};

type FirebaseAdminError = {
  readonly code: string;
  readonly message?: string;
};

const sessionCookieName = "admin_session";
const sessionCookieMaxAgeSeconds = 60 * 60 * 24;
const emulatorApiKey = "demo-api-key";

/**
 * Check whether the value is a record.
 */
const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

/**
 * Parse the JSON request body safely.
 */
const parseRequestBody = async (
  request: NextRequest,
): Promise<E2EAuthRequest> => {
  try {
    const json: unknown = await request.json();

    if (!isRecord(json)) {
      return {};
    }

    const uid = typeof json.uid === "string" ? json.uid : undefined;
    const email = typeof json.email === "string" ? json.email : undefined;

    return { uid, email };
  } catch {
    return {};
  }
};

/**
 * Resolve the E2E auth payload from request or environment.
 */
const resolveAuthPayload = (
  input: E2EAuthRequest,
): E2EAuthPayload | null => {
  const rawUid = input.uid ?? process.env.E2E_AUTH_UID;
  const rawEmail = input.email ?? process.env.E2E_AUTH_EMAIL;
  const uid = rawUid?.trim();
  const email = rawEmail?.trim();

  if (!uid || !email) {
    return null;
  }

  return { uid, email };
};

/**
 * Resolve the Firebase API key for Identity Toolkit.
 */
const resolveFirebaseApiKey = (useEmulator: boolean): string | null => {
  if (useEmulator) {
    return emulatorApiKey;
  }

  return process.env.NEXT_PUBLIC_FIREBASE_API_KEY ?? null;
};

/**
 * Resolve the Identity Toolkit base URL.
 */
const resolveIdentityToolkitBaseUrl = (useEmulator: boolean): string =>
  useEmulator
    ? "http://localhost:9099/identitytoolkit.googleapis.com"
    : "https://identitytoolkit.googleapis.com";

/**
 * Check whether an error is a Firebase Admin error.
 */
const isFirebaseAdminError = (error: unknown): error is FirebaseAdminError =>
  isRecord(error) && typeof error.code === "string";

/**
 * Check whether the error indicates a missing user.
 */
const isUserNotFoundError = (error: unknown): boolean =>
  isFirebaseAdminError(error) && error.code === "auth/user-not-found";

/**
 * Ensure the E2E test user exists in Firebase Auth.
 */
const ensureTestUser = async (
  auth: AdminAuth,
  payload: E2EAuthPayload,
): Promise<void> => {
  try {
    const user = await auth.getUser(payload.uid);

    if (user.email !== payload.email) {
      await auth.updateUser(payload.uid, {
        email: payload.email,
        emailVerified: true,
      });
    }
  } catch (error) {
    if (!isUserNotFoundError(error)) {
      throw error;
    }

    await auth.createUser({
      uid: payload.uid,
      email: payload.email,
      emailVerified: true,
    });
  }
};

/**
 * Exchange a custom token for an ID token using Identity Toolkit.
 */
const exchangeCustomTokenForIdToken = async (
  customToken: string,
  apiKey: string,
  useEmulator: boolean,
): Promise<string> => {
  const baseUrl = resolveIdentityToolkitBaseUrl(useEmulator);
  const response = await fetch(
    `${baseUrl}/v1/accounts:signInWithCustomToken?key=${apiKey}`,
    {
      method: "POST",
      headers: {
        "content-type": "application/json",
      },
      body: JSON.stringify({
        token: customToken,
        returnSecureToken: true,
      }),
    },
  );

  if (!response.ok) {
    const message = await response.text();
    throw new Error(`Failed to exchange token: ${message}`);
  }

  const payload: unknown = await response.json();

  if (!isRecord(payload) || typeof payload.idToken !== "string") {
    throw new Error("Invalid identity toolkit response");
  }

  return payload.idToken;
};

/**
 * Create a session cookie after verifying the ID token.
 */
const createSessionCookie = async (idToken: string): Promise<string> => {
  const result = await OIDCServerProvider.verifyIdToken(idToken)
    .andThen(() => OIDCServerProvider.createSessionCookie(idToken))
    .match<
      | { ok: true; cookie: string }
      | { ok: false; error: unknown }
    >({
      ok: (cookie) => ({ ok: true, cookie }),
      err: (error) => ({ ok: false, error }),
    });

  if (!result.ok) {
    throw result.error;
  }

  return result.cookie;
};

/**
 * Resolve an HTTP status for authentication failures.
 */
const resolveAuthFailureStatus = (error: unknown): number => {
  if (isOidcPermissionDeniedError(error)) {
    return 403;
  }

  if (isInvalidCredentialError(error) || isUserDisabledError(error)) {
    return 401;
  }

  return 500;
};

/**
 * Handle E2E authentication for Playwright.
 */
export async function POST(request: NextRequest): Promise<NextResponse> {
  const availability = isE2EAuthAvailable({
    e2eAuthEnabled: process.env.E2E_AUTH_ENABLED,
    useFirebaseEmulator: process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR,
  });

  if (!availability.available) {
    return new NextResponse("Not found", { status: 404 });
  }

  const secret = process.env.E2E_AUTH_SECRET;

  if (!secret) {
    return NextResponse.json(
      { error: "E2E auth is not configured" },
      { status: 500 },
    );
  }

  const providedSecret = request.headers.get("x-e2e-auth-secret");

  if (providedSecret !== secret) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const payload = resolveAuthPayload(await parseRequestBody(request));

  if (!payload) {
    return NextResponse.json(
      { error: "UID and email are required" },
      { status: 400 },
    );
  }

  const useEmulator = process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true";
  const apiKey = resolveFirebaseApiKey(useEmulator);

  if (!apiKey) {
    return NextResponse.json(
      { error: "Firebase API key is required" },
      { status: 500 },
    );
  }

  try {
    const auth = FirebaseAdminProvider.auth.instance;

    await ensureTestUser(auth, payload);

    const customToken = await auth.createCustomToken(payload.uid);
    const idToken = await exchangeCustomTokenForIdToken(
      customToken,
      apiKey,
      useEmulator,
    );
    const sessionCookie = await createSessionCookie(idToken);

    const response = NextResponse.json({ ok: true });

    response.cookies.set(sessionCookieName, sessionCookie, {
      httpOnly: true,
      secure: process.env.NODE_ENV === "production",
      sameSite: "strict",
      maxAge: sessionCookieMaxAgeSeconds,
      path: "/",
    });

    return response;
  } catch (error) {
    const status = resolveAuthFailureStatus(error);
    const message =
      error instanceof Error ? error.message : "Authentication failed";
    return NextResponse.json({ error: message }, { status });
  }
}
