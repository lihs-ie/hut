import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok, err } from "@/aspects/result";
import { unexpectedError, aggregateNotFoundError } from "@/aspects/error";
import type { Logger } from "@/aspects/logger";
import {
  type AuthContext,
  type AdminAuthConfig,
  registrationStart,
  registrationFinish,
  authenticationStart,
  authenticationFinish,
  logout,
  validateSessionAuth,
  authenticationSuccess,
} from "@/aspects/auth/workflows";
import {
  validateSession,
  type AdminSession,
  type SessionStorage,
} from "@/aspects/auth/session";
import {
  validatePasskeyAuthenticator,
  authenticationFailedError,
  type PasskeyStorage,
  type PasskeyAuth,
  type PasskeyAuthConfig,
  type PasskeyAuthenticator,
} from "@/aspects/auth/passkey";

// =============================================================================
// モックファクトリー
// =============================================================================

const createMockLogger = (): Logger => ({
  info: vi.fn(),
  warn: vi.fn(),
  error: vi.fn(),
  debug: vi.fn(),
});

const createMockPasskeyConfig = (): PasskeyAuthConfig => ({
  userId: "user-123",
  userName: "admin@example.com",
  userDisplayName: "Admin User",
  rpId: "example.com",
  rpName: "My Blog",
  origin: "https://example.com",
});

const createMockAdminAuthConfig = (): AdminAuthConfig => ({
  admin: {
    identifier: "admin-001",
    name: "Test Admin",
  },
  passkey: createMockPasskeyConfig(),
});

const createMockAuthenticator = (): PasskeyAuthenticator => {
  return validatePasskeyAuthenticator({
    credentialId: "cred-123",
    credentialPublicKey: "public-key-base64",
    counter: 5,
    transports: ["internal"],
    createdAt: new Date(),
  }).unwrap();
};

const createMockSession = (): AdminSession => {
  const now = new Date();
  const expiresAt = new Date(now.getTime() + 24 * 60 * 60 * 1000);
  return validateSession({
    id: "session-123",
    adminIdentifier: "admin-001",
    expiresAt,
    createdAt: now,
  }).unwrap();
};

const createExpiredSession = (): AdminSession => {
  const now = new Date();
  const pastDate = new Date(now.getTime() - 1000);
  const createdAt = new Date(now.getTime() - 24 * 60 * 60 * 1000);
  return validateSession({
    id: "session-expired",
    adminIdentifier: "admin-001",
    expiresAt: pastDate,
    createdAt,
  }).unwrap();
};

// =============================================================================
// テスト
// =============================================================================

describe("aspects/auth/workflows", () => {
  let mockLogger: Logger;
  let mockSessionStorage: SessionStorage;
  let mockPasskeyStorage: PasskeyStorage;
  let mockPasskeyAuth: PasskeyAuth;
  let mockAdminAuthConfig: AdminAuthConfig;

  beforeEach(() => {
    mockLogger = createMockLogger();
    mockAdminAuthConfig = createMockAdminAuthConfig();

    // デフォルトのモック実装
    mockSessionStorage = {
      persist: vi.fn(() => ok(undefined).toAsync()),
      find: vi.fn(() => ok(createMockSession()).toAsync()),
      terminate: vi.fn(() => ok(undefined).toAsync()),
    };

    mockPasskeyStorage = {
      persist: vi.fn(() => ok(undefined).toAsync()),
      findByCredentialId: vi.fn(() => ok(createMockAuthenticator()).toAsync()),
      findAll: vi.fn(() => ok([createMockAuthenticator()]).toAsync()),
      updateCounter: vi.fn(() => ok(undefined).toAsync()),
      terminate: vi.fn(() => ok(undefined).toAsync()),
    };

    mockPasskeyAuth = {
      generateRegistrationOptions: vi.fn(() =>
        ok({
          challenge: "challenge-123",
          rpId: "example.com",
          rpName: "My Blog",
          userId: "user-123",
          userName: "admin@example.com",
          userDisplayName: "Admin User",
        }).toAsync()
      ),
      verifyRegistration: vi.fn(() =>
        ok(createMockAuthenticator()).toAsync()
      ),
      generateAuthenticationOptions: vi.fn(() =>
        ok({
          challenge: "challenge-456",
          rpId: "example.com",
          allowCredentials: [{ id: "cred-123", type: "public-key" as const }],
        }).toAsync()
      ),
      verifyAuthentication: vi.fn(() =>
        ok({ verified: true, newCounter: 6 }).toAsync()
      ),
    };
  });

  const createContext = (): AuthContext => ({
    sessionStorage: mockSessionStorage,
    passkeyStorage: mockPasskeyStorage,
    passkeyAuth: mockPasskeyAuth,
    adminAuthConfig: mockAdminAuthConfig,
    logger: mockLogger,
  });

  describe("registrationStart", () => {
    it("登録オプションを生成できる", async () => {
      const ctx = createContext();
      const workflow = registrationStart(ctx);

      const result = await workflow({
        timestamp: new Date(),
        payload: {},
      }).unwrap();

      expect(result.options.challenge).toBe("challenge-123");
      expect(result.options.rpId).toBe("example.com");
      expect(result.challenge).toBe("challenge-123");
    });

    it("エラー時にログを出力する", async () => {
      mockPasskeyAuth.generateRegistrationOptions = vi.fn(() =>
        err(unexpectedError("Generation failed")).toAsync()
      );

      const ctx = createContext();
      const workflow = registrationStart(ctx);

      try {
        await workflow({ timestamp: new Date(), payload: {} }).unwrap();
      } catch {
        // Expected to throw
      }

      expect(mockLogger.error).toHaveBeenCalled();
    });
  });

  describe("registrationFinish", () => {
    it("登録を完了できる", async () => {
      const ctx = createContext();
      const workflow = registrationFinish(ctx);

      const result = await workflow({
        timestamp: new Date(),
        payload: {
          expectedChallenge: "challenge-123",
          response: {
            credentialId: "cred-123",
            clientDataJSON: "client-data",
            attestationObject: "attestation",
            transports: ["internal"],
          },
        },
      }).unwrap();

      expect(result.credentialId).toBe("cred-123");
      expect(mockPasskeyStorage.persist).toHaveBeenCalled();
    });

    it("検証失敗時にエラーを返す", async () => {
      mockPasskeyAuth.verifyRegistration = vi.fn(() =>
        err(authenticationFailedError("Verification failed")).toAsync()
      );

      const ctx = createContext();
      const workflow = registrationFinish(ctx);

      const isError = await workflow({
        timestamp: new Date(),
        payload: {
          expectedChallenge: "challenge-123",
          response: {
            credentialId: "cred-123",
            clientDataJSON: "client-data",
            attestationObject: "attestation",
          },
        },
      }).match({
        ok: () => false,
        err: () => true,
      });

      expect(isError).toBe(true);
    });
  });

  describe("authenticationStart", () => {
    it("認証オプションを生成できる", async () => {
      const ctx = createContext();
      const workflow = authenticationStart(ctx);

      const result = await workflow({
        timestamp: new Date(),
        payload: {},
      }).unwrap();

      expect(result.options.challenge).toBe("challenge-456");
      expect(result.options.allowCredentials.length).toBe(1);
    });

    it("認証器がない場合も動作する", async () => {
      mockPasskeyStorage.findAll = vi.fn(() => ok([]).toAsync());
      mockPasskeyAuth.generateAuthenticationOptions = vi.fn(() =>
        ok({
          challenge: "challenge-789",
          rpId: "example.com",
          allowCredentials: [],
        }).toAsync()
      );

      const ctx = createContext();
      const workflow = authenticationStart(ctx);

      const result = await workflow({
        timestamp: new Date(),
        payload: {},
      }).unwrap();

      expect(result.options.allowCredentials.length).toBe(0);
    });
  });

  describe("authenticationFinish", () => {
    it("認証を完了しセッションを作成できる", async () => {
      const ctx = createContext();
      const workflow = authenticationFinish(ctx);

      const result = await workflow({
        timestamp: new Date(),
        payload: {
          expectedChallenge: "challenge-456",
          response: {
            credentialId: "cred-123",
            clientDataJSON: "client-data",
            authenticatorData: "auth-data",
            signature: "signature",
          },
        },
      }).unwrap();

      expect(result._tag).toBe("AuthenticationSuccess");
      expect(result.admin.identifier).toBe("admin-001");
      expect(result.session).toBeDefined();
      expect(mockSessionStorage.persist).toHaveBeenCalled();
      expect(mockPasskeyStorage.updateCounter).toHaveBeenCalled();
    });

    it("認証器が見つからない場合エラーを返す", async () => {
      mockPasskeyStorage.findByCredentialId = vi.fn(() =>
        err(
          aggregateNotFoundError("PasskeyAuthenticator", "Not found")
        ).toAsync()
      );

      const ctx = createContext();
      const workflow = authenticationFinish(ctx);

      const isError = await workflow({
        timestamp: new Date(),
        payload: {
          expectedChallenge: "challenge-456",
          response: {
            credentialId: "unknown-cred",
            clientDataJSON: "client-data",
            authenticatorData: "auth-data",
            signature: "signature",
          },
        },
      }).match({
        ok: () => false,
        err: () => true,
      });

      expect(isError).toBe(true);
    });

    it("認証検証が失敗した場合エラーを返す", async () => {
      mockPasskeyAuth.verifyAuthentication = vi.fn(() =>
        ok({ verified: false, newCounter: 6 }).toAsync()
      );

      const ctx = createContext();
      const workflow = authenticationFinish(ctx);

      const isError = await workflow({
        timestamp: new Date(),
        payload: {
          expectedChallenge: "challenge-456",
          response: {
            credentialId: "cred-123",
            clientDataJSON: "client-data",
            authenticatorData: "auth-data",
            signature: "signature",
          },
        },
      }).match({
        ok: () => false,
        err: () => true,
      });

      expect(isError).toBe(true);
    });
  });

  describe("logout", () => {
    it("セッションを削除できる", async () => {
      const ctx = createContext();
      const workflow = logout(ctx);

      await workflow({
        timestamp: new Date(),
        payload: { sessionId: "session-123" },
      }).unwrap();

      expect(mockSessionStorage.terminate).toHaveBeenCalled();
    });

    it("セッションが見つからない場合エラーを返す", async () => {
      mockSessionStorage.find = vi.fn(() =>
        err(aggregateNotFoundError("AdminSession", "Not found")).toAsync()
      );

      const ctx = createContext();
      const workflow = logout(ctx);

      const isError = await workflow({
        timestamp: new Date(),
        payload: { sessionId: "unknown-session" },
      }).match({
        ok: () => false,
        err: () => true,
      });

      expect(isError).toBe(true);
    });

    it("期限切れセッションはエラーを返す", async () => {
      mockSessionStorage.find = vi.fn(() =>
        ok(createExpiredSession()).toAsync()
      );

      const ctx = createContext();
      const workflow = logout(ctx);

      const isError = await workflow({
        timestamp: new Date(),
        payload: { sessionId: "session-expired" },
      }).match({
        ok: () => false,
        err: () => true,
      });

      expect(isError).toBe(true);
    });
  });

  describe("validateSessionAuth", () => {
    it("有効なセッションで管理者情報を返す", async () => {
      const ctx = createContext();
      const workflow = validateSessionAuth(ctx);

      const result = await workflow({
        timestamp: new Date(),
        payload: { sessionId: "session-123" },
      }).unwrap();

      expect(result.identifier).toBe("admin-001");
      expect(result.name).toBe("Test Admin");
    });

    it("セッションが見つからない場合エラーを返す", async () => {
      mockSessionStorage.find = vi.fn(() =>
        err(aggregateNotFoundError("AdminSession", "Not found")).toAsync()
      );

      const ctx = createContext();
      const workflow = validateSessionAuth(ctx);

      const isError = await workflow({
        timestamp: new Date(),
        payload: { sessionId: "unknown-session" },
      }).match({
        ok: () => false,
        err: () => true,
      });

      expect(isError).toBe(true);
    });

    it("期限切れセッションはエラーを返す", async () => {
      mockSessionStorage.find = vi.fn(() =>
        ok(createExpiredSession()).toAsync()
      );

      const ctx = createContext();
      const workflow = validateSessionAuth(ctx);

      const isError = await workflow({
        timestamp: new Date(),
        payload: { sessionId: "session-expired" },
      }).match({
        ok: () => false,
        err: () => true,
      });

      expect(isError).toBe(true);
    });
  });

  describe("authenticationSuccess", () => {
    it("AuthenticationSuccessオブジェクトを作成できる", () => {
      const admin = { identifier: "admin-001", name: "Test Admin" };
      const session = createMockSession();

      const result = authenticationSuccess(admin, session);

      expect(result._tag).toBe("AuthenticationSuccess");
      expect(result.admin).toBe(admin);
      expect(result.session).toBe(session);
    });
  });
});
