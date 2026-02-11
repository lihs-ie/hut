/**
 * Route Handler のロジックをテスト用にインポート可能にするモジュール。
 * 実際の reader/src/app/api/engagement/route.ts と同一ロジック。
 * テスト環境で next/server の NextRequest/NextResponse をモック不要にするため、
 * 標準の Response を使用する。
 */
import { cookies, headers } from "next/headers";
import { z } from "zod";
import { FirebaseProvider } from "@shared/providers/infrastructure/firebase";
import { getJstDateKey } from "@shared/aspects/date";

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
  const allowedHosts = [
    process.env.NEXT_PUBLIC_SITE_URL ?? "http://localhost:3000",
  ].filter(Boolean);
  const target = origin ?? referer;
  if (!target) return false;

  try {
    const url = new URL(target);
    return allowedHosts.some((host) => url.origin === new URL(host).origin);
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
    const document = operations.doc(firestore, "rate-limits", key);
    const snapshot = await transaction.get(document);
    const now = new Date();

    const data = snapshot.data() as RateLimitData | undefined;

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

const resolveIpAddress = (requestHeaders: {
  get: (name: string) => string | null;
}): string => {
  const forwarded = requestHeaders.get("x-forwarded-for");
  if (forwarded) return forwarded.split(",")[0].trim();

  return requestHeaders.get("x-real-ip") ?? "unknown";
};

type EngagementRequest = {
  json: () => Promise<unknown>;
};

type EngagementResponse = {
  status: number;
  json: () => Promise<{ status: string }>;
};

function jsonResponse(
  body: { status: string },
  options?: { status?: number },
): EngagementResponse {
  const statusCode = options?.status ?? 200;
  return {
    status: statusCode,
    json: () => Promise.resolve(body),
  };
}

export async function POST(
  request: EngagementRequest,
): Promise<EngagementResponse> {
  // 1. Origin チェック
  const requestHeaders = await headers();
  const origin = requestHeaders.get("origin");
  const referer = requestHeaders.get("referer");

  if (!isAllowedOrigin(origin, referer)) {
    return jsonResponse({ status: "forbidden" }, { status: 403 });
  }

  // 2. セッション検証
  const cookieStore = await cookies();

  if (cookieStore.get(adminSessionCookieName)?.value) {
    return jsonResponse({ status: "skipped" });
  }

  const sessionKey = cookieStore.get(viewSessionCookieName)?.value;
  if (!sessionKey) {
    return jsonResponse({ status: "no_session" }, { status: 401 });
  }

  // 3. レート制限
  const ipAddress = resolveIpAddress(requestHeaders);
  const allowed = await checkRateLimit(ipAddress);

  if (!allowed) {
    return jsonResponse({ status: "rate_limited" }, { status: 429 });
  }

  // 4. 入力バリデーション
  const parseResult = bodySchema.safeParse(await request.json());

  if (!parseResult.success) {
    return jsonResponse({ status: "invalid" }, { status: 400 });
  }

  const body = parseResult.data;

  // 5. Firestore 書き込み
  const { instance: firestore, operations } = FirebaseProvider.firestore;
  const now = new Date();
  const dateKey = getJstDateKey(now);
  const documentId = `${dateKey}:${sessionKey}`;

  const document = operations.doc(
    firestore,
    "engagement-logs",
    body.contentType,
    body.contentIdentifier,
    documentId,
  );

  await operations.setDoc(
    document,
    {
      dwellTime: body.dwellTime,
      maxScrollDepth: body.maxScrollDepth,
      createdAt: now.toISOString(),
      updatedAt: now.toISOString(),
    },
    { merge: true },
  );

  return jsonResponse({ status: "ok" });
}
