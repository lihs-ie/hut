import { cookies, headers } from "next/headers";
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";
import { FirebaseProvider } from "@shared/providers/infrastructure/firebase";
import { EngagementWorkflowProvider } from "@shared/providers/workflows/analytics/engagement";

const viewSessionCookieName = "pv_session";
const adminSessionCookieName = "admin_session";

const RATE_LIMIT_WINDOW_MS = 60_000;
const RATE_LIMIT_MAX = 30;

const bodySchema = z.object({
  contentType: z.enum(["article", "memo", "series"]),
  contentIdentifier: z.string().ulid(),
  dwellTime: z.number().int().min(0).max(86_400),
  maxScrollDepth: z.number().int().min(0).max(100),
});

const isAllowedOrigin = (
  origin: string | null,
  referer: string | null,
): boolean => {
  const allowedHosts = [process.env.NEXT_PUBLIC_SITE_URL].filter(Boolean);
  const target = origin ?? referer;
  if (!target) return false;

  try {
    const url = new URL(target);
    return allowedHosts.some(
      (host) => url.origin === new URL(host!).origin,
    );
  } catch {
    return false;
  }
};

type RateLimitData = {
  count: number;
  windowStart: string;
};

const checkRateLimit = async (ipAddress: string): Promise<boolean> => {
  const { instance: firestore, operations } = FirebaseProvider.firestore;
  const key = `engagement:${ipAddress}`;

  return await operations.runTransaction(firestore, async (transaction) => {
    const document = operations.doc<RateLimitData>(firestore, "rate-limits", key);
    const snapshot = await transaction.get(document);
    const now = new Date();

    const data = snapshot.data();

    let currentCount = 0;
    let windowStart = now;

    if (data) {
      const elapsed = now.getTime() - new Date(data.windowStart).getTime();
      if (elapsed < RATE_LIMIT_WINDOW_MS) {
        currentCount = data.count;
        windowStart = new Date(data.windowStart);
      }
    }

    if (currentCount >= RATE_LIMIT_MAX) {
      return false;
    }

    transaction.set(
      document,
      {
        count: currentCount + 1,
        windowStart: windowStart.toISOString(),
      },
      { merge: true },
    );

    return true;
  });
};

const resolveIpAddress = (requestHeaders: Headers): string => {
  const forwarded = requestHeaders.get("x-forwarded-for");
  if (forwarded) return forwarded.split(",")[0].trim();

  return requestHeaders.get("x-real-ip") ?? "unknown";
};

export async function POST(request: NextRequest): Promise<NextResponse> {
  const requestHeaders = await headers();
  const origin = requestHeaders.get("origin");
  const referer = requestHeaders.get("referer");

  if (!isAllowedOrigin(origin, referer)) {
    return NextResponse.json({ status: "forbidden" }, { status: 403 });
  }

  const cookieStore = await cookies();

  if (cookieStore.get(adminSessionCookieName)?.value) {
    return NextResponse.json({ status: "skipped" });
  }

  const sessionKey = cookieStore.get(viewSessionCookieName)?.value;
  if (!sessionKey) {
    return NextResponse.json({ status: "no_session" }, { status: 401 });
  }

  const ipAddress = resolveIpAddress(requestHeaders);
  const allowed = await checkRateLimit(ipAddress);

  if (!allowed) {
    return NextResponse.json({ status: "rate_limited" }, { status: 429 });
  }

  const parseResult = bodySchema.safeParse(await request.json());

  if (!parseResult.success) {
    return NextResponse.json({ status: "invalid" }, { status: 400 });
  }

  const body = parseResult.data;

  return EngagementWorkflowProvider.record({
    now: new Date(),
    payload: {
      reference: {
        type: body.contentType,
        content: body.contentIdentifier,
      },
      sessionKey,
      dwellTime: body.dwellTime,
      scrollDepth: body.maxScrollDepth,
    },
  }).match({
    ok: () => NextResponse.json({ status: "ok" }),
    err: () => NextResponse.json({ status: "error" }, { status: 500 }),
  });
}
