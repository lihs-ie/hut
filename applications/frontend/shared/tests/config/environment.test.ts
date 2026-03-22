import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import { Environment } from "@shared/aspects/logger";

describe("config/environment", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    vi.resetModules();
    process.env = { ...originalEnv };
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  describe("resolveEnvironment", () => {
    it("NEXT_PUBLIC_APP_ENV=staging の場合 STAGING を返す", async () => {
      process.env.NEXT_PUBLIC_APP_ENV = "staging";
      const { resolveEnvironment } = await import(
        "@shared/config/environment"
      );
      expect(resolveEnvironment()).toBe(Environment.STAGING);
    });

    it("NEXT_PUBLIC_APP_ENV=production の場合 PRODUCTION を返す", async () => {
      process.env.NEXT_PUBLIC_APP_ENV = "production";
      const { resolveEnvironment } = await import(
        "@shared/config/environment"
      );
      expect(resolveEnvironment()).toBe(Environment.PRODUCTION);
    });

    it("NEXT_PUBLIC_APP_ENV=development の場合 DEVELOPMENT を返す", async () => {
      process.env.NEXT_PUBLIC_APP_ENV = "development";
      const { resolveEnvironment } = await import(
        "@shared/config/environment"
      );
      expect(resolveEnvironment()).toBe(Environment.DEVELOPMENT);
    });

    it("NEXT_PUBLIC_APP_ENV 未設定 + NODE_ENV=production の場合 PRODUCTION を返す", async () => {
      delete process.env.NEXT_PUBLIC_APP_ENV;
      process.env.NODE_ENV = "production";
      const { resolveEnvironment } = await import(
        "@shared/config/environment"
      );
      expect(resolveEnvironment()).toBe(Environment.PRODUCTION);
    });

    it("NEXT_PUBLIC_APP_ENV 未設定 + NODE_ENV=development の場合 DEVELOPMENT を返す", async () => {
      delete process.env.NEXT_PUBLIC_APP_ENV;
      process.env.NODE_ENV = "development";
      const { resolveEnvironment } = await import(
        "@shared/config/environment"
      );
      expect(resolveEnvironment()).toBe(Environment.DEVELOPMENT);
    });

    it("両方未設定の場合 DEVELOPMENT を返す", async () => {
      delete process.env.NEXT_PUBLIC_APP_ENV;
      delete process.env.NODE_ENV;
      const { resolveEnvironment } = await import(
        "@shared/config/environment"
      );
      expect(resolveEnvironment()).toBe(Environment.DEVELOPMENT);
    });
  });

  describe("shouldShowErrorDetails", () => {
    it("NEXT_PUBLIC_APP_ENV=staging の場合 true を返す", async () => {
      process.env.NEXT_PUBLIC_APP_ENV = "staging";
      const { shouldShowErrorDetails } = await import(
        "@shared/config/environment"
      );
      expect(shouldShowErrorDetails()).toBe(true);
    });

    it("NEXT_PUBLIC_APP_ENV=production の場合 false を返す", async () => {
      process.env.NEXT_PUBLIC_APP_ENV = "production";
      const { shouldShowErrorDetails } = await import(
        "@shared/config/environment"
      );
      expect(shouldShowErrorDetails()).toBe(false);
    });

    it("NEXT_PUBLIC_APP_ENV 未設定 + NODE_ENV=development の場合 true を返す", async () => {
      delete process.env.NEXT_PUBLIC_APP_ENV;
      process.env.NODE_ENV = "development";
      const { shouldShowErrorDetails } = await import(
        "@shared/config/environment"
      );
      expect(shouldShowErrorDetails()).toBe(true);
    });

    it("NEXT_PUBLIC_APP_ENV 未設定 + NODE_ENV=production の場合 false を返す", async () => {
      delete process.env.NEXT_PUBLIC_APP_ENV;
      process.env.NODE_ENV = "production";
      const { shouldShowErrorDetails } = await import(
        "@shared/config/environment"
      );
      expect(shouldShowErrorDetails()).toBe(false);
    });

    it("両方未設定の場合 true を返す", async () => {
      delete process.env.NEXT_PUBLIC_APP_ENV;
      delete process.env.NODE_ENV;
      const { shouldShowErrorDetails } = await import(
        "@shared/config/environment"
      );
      expect(shouldShowErrorDetails()).toBe(true);
    });
  });
});
