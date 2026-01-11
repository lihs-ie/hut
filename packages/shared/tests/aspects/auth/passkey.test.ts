import { describe, it, expect } from "vitest";
import {
  validatePasskeyAuthenticator,
  authenticationFailedError,
  isAuthenticationFailedError,
  type PasskeyAuthConfig,
} from "@shared/aspects/auth/passkey";

describe("aspects/auth/passkey", () => {
  describe("validatePasskeyAuthenticator", () => {
    it("有効な認証器データを検証できる", () => {
      const result = validatePasskeyAuthenticator({
        credentialId: "cred-123",
        credentialPublicKey: "public-key-base64",
        counter: 0,
        transports: ["internal", "usb"],
        createdAt: new Date(),
      });

      expect(result.isOk).toBe(true);
      const authenticator = result.unwrap();
      expect(authenticator.credentialId).toBe("cred-123");
      expect(authenticator.credentialPublicKey).toBe("public-key-base64");
      expect(authenticator.counter).toBe(0);
    });

    it("transportsなしでも検証できる", () => {
      const result = validatePasskeyAuthenticator({
        credentialId: "cred-123",
        credentialPublicKey: "public-key-base64",
        counter: 5,
        createdAt: new Date(),
      });

      expect(result.isOk).toBe(true);
      const authenticator = result.unwrap();
      expect(authenticator.transports).toBeUndefined();
    });

    it("空のcredentialIdでエラーを返す", () => {
      const result = validatePasskeyAuthenticator({
        credentialId: "",
        credentialPublicKey: "public-key-base64",
        counter: 0,
        createdAt: new Date(),
      });

      expect(result.isErr).toBe(true);
    });

    it("空のcredentialPublicKeyでエラーを返す", () => {
      const result = validatePasskeyAuthenticator({
        credentialId: "cred-123",
        credentialPublicKey: "",
        counter: 0,
        createdAt: new Date(),
      });

      expect(result.isErr).toBe(true);
    });

    it("負のcounterでエラーを返す", () => {
      const result = validatePasskeyAuthenticator({
        credentialId: "cred-123",
        credentialPublicKey: "public-key-base64",
        counter: -1,
        createdAt: new Date(),
      });

      expect(result.isErr).toBe(true);
    });
  });

  describe("authenticationFailedError", () => {
    it("AuthenticationFailedErrorを作成できる", () => {
      const error = authenticationFailedError("Invalid challenge");

      expect(error.reason).toBe("Invalid challenge");
      expect(error._tag).toBeDefined();
    });
  });

  describe("isAuthenticationFailedError", () => {
    it("AuthenticationFailedErrorを正しく判定する", () => {
      const error = authenticationFailedError("Test error");

      expect(isAuthenticationFailedError(error)).toBe(true);
    });

    it("他のオブジェクトはfalseを返す", () => {
      expect(isAuthenticationFailedError(null)).toBe(false);
      expect(isAuthenticationFailedError(undefined)).toBe(false);
      expect(isAuthenticationFailedError({})).toBe(false);
      expect(isAuthenticationFailedError({ _tag: Symbol("Other") })).toBe(
        false
      );
    });
  });

  describe("PasskeyAuthConfig", () => {
    it("正しい設定オブジェクトを作成できる", () => {
      const config: PasskeyAuthConfig = {
        userId: "user-123",
        userName: "admin@example.com",
        userDisplayName: "Admin User",
        rpId: "example.com",
        rpName: "My Blog",
        origin: "https://example.com",
      };

      expect(config.userId).toBe("user-123");
      expect(config.rpId).toBe("example.com");
      expect(config.origin).toBe("https://example.com");
    });
  });
});
