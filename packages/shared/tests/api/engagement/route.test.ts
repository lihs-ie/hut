/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

// next/headers のモック
const mockCookies = {
  get: vi.fn(),
};

const mockHeaders = new Map<string, string>();
const mockHeadersObject = {
  get: (key: string) => mockHeaders.get(key) ?? null,
};

vi.mock("next/headers", () => ({
  cookies: vi.fn(() => Promise.resolve(mockCookies)),
  headers: vi.fn(() => Promise.resolve(mockHeadersObject)),
}));

// Firestore のモック
const mockTransaction = {
  get: vi.fn(),
  set: vi.fn(),
};

const mockFirestore = {
  instance: {},
  operations: {
    doc: vi.fn().mockReturnValue({}),
    setDoc: vi.fn().mockResolvedValue(undefined),
    runTransaction: vi.fn((_firestore: unknown, callback: (transaction: typeof mockTransaction) => Promise<unknown>) =>
      callback(mockTransaction),
    ),
  },
};

vi.mock("@shared/providers/infrastructure/firebase", () => ({
  FirebaseProvider: {
    firestore: mockFirestore,
  },
}));

vi.mock("@shared/aspects/date", () => ({
  getJstDateKey: vi.fn(() => "2025-01-15"),
}));

/**
 * NextRequest のモックを生成する
 */
function createMockRequest(body: unknown): {
  json: () => Promise<unknown>;
} {
  return {
    json: () => Promise.resolve(body),
  };
}

/**
 * Cookie取得のモック実装を作成する
 */
function createCookieGetMock(options: {
  adminSession?: string;
  pageViewSession?: string;
}) {
  return (name: string) => {
    if (name === "admin_session") {
      return options.adminSession
        ? { value: options.adminSession }
        : undefined;
    }
    if (name === "pv_session") {
      return options.pageViewSession
        ? { value: options.pageViewSession }
        : undefined;
    }
    return undefined;
  };
}

/**
 * ヘッダーのモック設定
 */
function setupHeaders(headers: Record<string, string>) {
  mockHeaders.clear();
  for (const [key, value] of Object.entries(headers)) {
    mockHeaders.set(key, value);
  }
}

/**
 * レート制限のモック設定
 */
function setupRateLimitMock(options: {
  isLimited: boolean;
  existingCount?: number;
  existingWindowStart?: string;
}) {
  mockTransaction.get.mockResolvedValue({
    data: () => {
      if (options.existingCount !== undefined) {
        return {
          count: options.existingCount,
          windowStart:
            options.existingWindowStart ?? new Date().toISOString(),
        };
      }
      return undefined;
    },
  });
}

const validBody = {
  contentType: "article",
  contentIdentifier: "01JFABCDEF1234567890ABCDEF",
  dwellTime: 120,
  maxScrollDepth: 75,
};

describe("POST /api/engagement", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockHeaders.clear();
    mockCookies.get.mockReset();
    mockTransaction.get.mockReset();
    mockTransaction.set.mockReset();
    mockFirestore.operations.doc.mockReturnValue({});
    mockFirestore.operations.setDoc.mockResolvedValue(undefined);

    // デフォルトで有効なヘッダーとCookieを設定
    setupHeaders({
      origin: "http://localhost:3000",
      "x-forwarded-for": "192.168.1.1",
    });
    mockCookies.get.mockImplementation(
      createCookieGetMock({ pageViewSession: "test-session-key" }),
    );
    setupRateLimitMock({ isLimited: false });
  });

  describe("Originチェック", () => {
    it("Originヘッダーがない場合は403を返すこと", async () => {
      setupHeaders({});

      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest(validBody);
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(403);
      expect(data.status).toBe("forbidden");
    });

    it("不正なOriginの場合は403を返すこと", async () => {
      setupHeaders({ origin: "https://evil.example.com" });

      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest(validBody);
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(403);
      expect(data.status).toBe("forbidden");
    });

    it("Refererヘッダーのみでも有効な場合は通過すること", async () => {
      setupHeaders({
        referer: "http://localhost:3000/articles/test",
        "x-forwarded-for": "192.168.1.1",
      });
      mockCookies.get.mockImplementation(
        createCookieGetMock({ pageViewSession: "test-session-key" }),
      );
      setupRateLimitMock({ isLimited: false });

      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest(validBody);
      const response = await POST(request);
      const data = await response.json();

      expect(data.status).toBe("ok");
    });
  });

  describe("セッション検証", () => {
    it("管理者セッションがある場合はスキップすること", async () => {
      setupHeaders({
        origin: "http://localhost:3000",
        "x-forwarded-for": "192.168.1.1",
      });
      mockCookies.get.mockImplementation(
        createCookieGetMock({
          adminSession: "admin-session",
          pageViewSession: "test-session",
        }),
      );

      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest(validBody);
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(200);
      expect(data.status).toBe("skipped");
    });

    it("pv_sessionがない場合は401を返すこと", async () => {
      setupHeaders({
        origin: "http://localhost:3000",
        "x-forwarded-for": "192.168.1.1",
      });
      mockCookies.get.mockImplementation(createCookieGetMock({}));

      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest(validBody);
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(401);
      expect(data.status).toBe("no_session");
    });
  });

  describe("レート制限", () => {
    it("レート制限を超えた場合は429を返すこと", async () => {
      setupRateLimitMock({
        isLimited: true,
        existingCount: 30,
        existingWindowStart: new Date().toISOString(),
      });

      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest(validBody);
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(429);
      expect(data.status).toBe("rate_limited");
    });

    it("ウィンドウが期限切れの場合はカウントをリセットして許可すること", async () => {
      const expiredWindowStart = new Date(
        Date.now() - 120_000,
      ).toISOString();
      setupRateLimitMock({
        isLimited: false,
        existingCount: 30,
        existingWindowStart: expiredWindowStart,
      });

      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest(validBody);
      const response = await POST(request);
      const data = await response.json();

      expect(data.status).toBe("ok");
    });
  });

  describe("入力バリデーション", () => {
    it("不正なcontentTypeの場合は400を返すこと", async () => {
      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest({
        ...validBody,
        contentType: "invalid",
      });
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(400);
      expect(data.status).toBe("invalid");
    });

    it("不正なcontentIdentifier（ULID以外）の場合は400を返すこと", async () => {
      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest({
        ...validBody,
        contentIdentifier: "not-a-ulid",
      });
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(400);
      expect(data.status).toBe("invalid");
    });

    it("dwellTimeが負の値の場合は400を返すこと", async () => {
      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest({
        ...validBody,
        dwellTime: -1,
      });
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(400);
      expect(data.status).toBe("invalid");
    });

    it("maxScrollDepthが100を超える場合は400を返すこと", async () => {
      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest({
        ...validBody,
        maxScrollDepth: 101,
      });
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(400);
      expect(data.status).toBe("invalid");
    });

    it("dwellTimeが86400を超える場合は400を返すこと", async () => {
      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest({
        ...validBody,
        dwellTime: 86401,
      });
      const response = await POST(request);
      const data = await response.json();

      expect(response.status).toBe(400);
      expect(data.status).toBe("invalid");
    });

    it("memoタイプのcontentTypeが有効であること", async () => {
      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest({
        ...validBody,
        contentType: "memo",
      });
      const response = await POST(request);
      const data = await response.json();

      expect(data.status).toBe("ok");
    });

    it("seriesタイプのcontentTypeが有効であること", async () => {
      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest({
        ...validBody,
        contentType: "series",
      });
      const response = await POST(request);
      const data = await response.json();

      expect(data.status).toBe("ok");
    });
  });

  describe("Firestore書き込み", () => {
    it("正常なリクエストでFirestoreにエンゲージメントログを書き込むこと", async () => {
      const { POST } = await import(
        "./route-handler"
      );
      const request = createMockRequest(validBody);
      const response = await POST(request);
      const data = await response.json();

      expect(data.status).toBe("ok");
      expect(mockFirestore.operations.doc).toHaveBeenCalledWith(
        mockFirestore.instance,
        "engagement-logs",
        "article",
        "01JFABCDEF1234567890ABCDEF",
        expect.stringContaining("2025-01-15:test-session-key"),
      );
      expect(mockFirestore.operations.setDoc).toHaveBeenCalledWith(
        expect.anything(),
        expect.objectContaining({
          dwellTime: 120,
          maxScrollDepth: 75,
        }),
        { merge: true },
      );
    });
  });
});
