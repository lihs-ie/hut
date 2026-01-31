import { describe, it, expect } from "vitest";
import {
  validateSession,
  isSessionExpired,
  isSessionValid,
  sessionExpiredError,
  isSessionExpiredError,
  type AdminSession,
  type SessionId,
} from "@shared/aspects/auth/session";

describe("aspects/auth/session", () => {
  describe("validateSession", () => {
    it("有効なセッションデータを検証できる", () => {
      const now = new Date();
      const expiresAt = new Date(now.getTime() + 24 * 60 * 60 * 1000);

      const result = validateSession({
        id: "session-123",
        adminIdentifier: "admin-001",
        expiresAt,
        createdAt: now,
      });

      expect(result.isOk).toBe(true);
      const session = result.unwrap();
      expect(session.id).toBe("session-123");
      expect(session.adminIdentifier).toBe("admin-001");
    });

    it("空のセッションIDでエラーを返す", () => {
      const now = new Date();
      const expiresAt = new Date(now.getTime() + 24 * 60 * 60 * 1000);

      const result = validateSession({
        id: "",
        adminIdentifier: "admin-001",
        expiresAt,
        createdAt: now,
      });

      expect(result.isErr).toBe(true);
    });

    it("空の管理者識別子でエラーを返す", () => {
      const now = new Date();
      const expiresAt = new Date(now.getTime() + 24 * 60 * 60 * 1000);

      const result = validateSession({
        id: "session-123",
        adminIdentifier: "",
        expiresAt,
        createdAt: now,
      });

      expect(result.isErr).toBe(true);
    });
  });

  describe("isSessionExpired", () => {
    it("有効期限が過ぎたセッションはtrueを返す", () => {
      const now = new Date();
      const pastDate = new Date(now.getTime() - 1000); // 1秒前
      const createdAt = new Date(now.getTime() - 24 * 60 * 60 * 1000);

      const session = validateSession({
        id: "session-123",
        adminIdentifier: "admin-001",
        expiresAt: pastDate,
        createdAt,
      }).unwrap();

      expect(isSessionExpired(session)).toBe(true);
    });

    it("有効期限内のセッションはfalseを返す", () => {
      const now = new Date();
      const futureDate = new Date(now.getTime() + 24 * 60 * 60 * 1000);

      const session = validateSession({
        id: "session-123",
        adminIdentifier: "admin-001",
        expiresAt: futureDate,
        createdAt: now,
      }).unwrap();

      expect(isSessionExpired(session)).toBe(false);
    });
  });

  describe("isSessionValid", () => {
    it("有効期限内のセッションはtrueを返す", () => {
      const now = new Date();
      const futureDate = new Date(now.getTime() + 24 * 60 * 60 * 1000);

      const session = validateSession({
        id: "session-123",
        adminIdentifier: "admin-001",
        expiresAt: futureDate,
        createdAt: now,
      }).unwrap();

      expect(isSessionValid(session)).toBe(true);
    });

    it("有効期限が過ぎたセッションはfalseを返す", () => {
      const now = new Date();
      const pastDate = new Date(now.getTime() - 1000);
      const createdAt = new Date(now.getTime() - 24 * 60 * 60 * 1000);

      const session = validateSession({
        id: "session-123",
        adminIdentifier: "admin-001",
        expiresAt: pastDate,
        createdAt,
      }).unwrap();

      expect(isSessionValid(session)).toBe(false);
    });
  });

  describe("sessionExpiredError", () => {
    it("SessionExpiredErrorを作成できる", () => {
      const sessionId = "session-123" as SessionId;
      const error = sessionExpiredError(sessionId);

      expect(error.sessionId).toBe(sessionId);
      expect(error._tag).toBeDefined();
    });
  });

  describe("isSessionExpiredError", () => {
    it("SessionExpiredErrorを正しく判定する", () => {
      const sessionId = "session-123" as SessionId;
      const error = sessionExpiredError(sessionId);

      expect(isSessionExpiredError(error)).toBe(true);
    });

    it("他のオブジェクトはfalseを返す", () => {
      expect(isSessionExpiredError(null)).toBe(false);
      expect(isSessionExpiredError(undefined)).toBe(false);
      expect(isSessionExpiredError({})).toBe(false);
      expect(isSessionExpiredError({ _tag: Symbol("Other") })).toBe(false);
    });
  });
});
