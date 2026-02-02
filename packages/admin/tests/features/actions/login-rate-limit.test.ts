/**
 * Login Rate Limit Action Feature Test
 *
 * Firebase Emulatorを使用してログインレート制限アクションの統合テストを行います。
 *
 * 注意: このテストはFirebaseAdminProviderを使用するため、
 * 環境変数FIRESTORE_EMULATOR_HOSTが設定されている必要があります。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import {
  createAdminFeatureTestContext,
  cleanupAdminFeatureTest,
  getTestFirestore,
} from "../setup";
import type { Firestore } from "firebase-admin/firestore";

/**
 * enforceLoginRateLimitのコアロジックをテスト用に抽出
 * (Server Actionは直接インポートできないため)
 */
import { enforceRateLimit } from "@/aspects/rate-limit";
import { isResourceExhaustedError } from "@shared/aspects/error";

type RateLimitResult =
  | { allowed: true }
  | { allowed: false; message: string };

const loginRateLimitWindowMs = 10 * 60 * 1000;
const loginRateLimitMaxAttempts = 5;

const normalizeRateLimitKey = (key: string): string => {
  const trimmed = key.trim();

  if (trimmed.length === 0) {
    return "unknown";
  }

  return trimmed;
};

const enforceLoginRateLimitCore = async (
  firestore: Firestore,
  key: string
): Promise<RateLimitResult> => {
  const normalizedKey = normalizeRateLimitKey(key);

  return await enforceRateLimit(firestore, {
    key: `admin_login:${normalizedKey}`,
    limit: loginRateLimitMaxAttempts,
    windowMs: loginRateLimitWindowMs,
  }).match<RateLimitResult>({
    ok: (): RateLimitResult => ({ allowed: true }),
    err: (error): RateLimitResult => {
      if (isResourceExhaustedError(error)) {
        return { allowed: false, message: error.message };
      }

      return { allowed: true };
    },
  });
};

describe("Feature: Login Rate Limit Action (実DB接続)", () => {
  let firestore: Firestore;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    await createAdminFeatureTestContext();
    firestore = getTestFirestore();
  });

  afterAll(async () => {
    await cleanupAdminFeatureTest();
  });

  describe("ログインレート制限の動作", () => {
    it("最初のログイン試行は許可される", async () => {
      const key = `user-${Date.now()}@example.com`;

      const result = await enforceLoginRateLimitCore(firestore, key);

      expect(result.allowed).toBe(true);
    });

    it("5回までのログイン試行は許可される", async () => {
      const key = `user-limit-${Date.now()}@example.com`;

      for (let i = 0; i < 5; i++) {
        const result = await enforceLoginRateLimitCore(firestore, key);
        expect(result.allowed).toBe(true);
      }
    });

    it("6回目のログイン試行は拒否される", async () => {
      const key = `user-exceed-${Date.now()}@example.com`;

      for (let i = 0; i < 5; i++) {
        await enforceLoginRateLimitCore(firestore, key);
      }

      const result = await enforceLoginRateLimitCore(firestore, key);

      expect(result.allowed).toBe(false);
      if (!result.allowed) {
        expect(result.message).toBe(
          "ログイン試行が多すぎます。しばらくしてから再試行してください。"
        );
      }
    });
  });

  describe("キーの正規化", () => {
    it("空白を含むキーがトリムされる", async () => {
      const keyWithSpaces = `  user-spaces-${Date.now()}@example.com  `;
      const keyTrimmed = `user-spaces-${Date.now()}@example.com`;

      await enforceLoginRateLimitCore(firestore, keyWithSpaces);

      const docRef = firestore
        .collection("admin-rate-limits")
        .doc(`admin_login:${keyTrimmed}`);
      const snapshot = await docRef.get();

      expect(snapshot.exists).toBe(true);
    });

    it("空のキーは'unknown'に変換される", async () => {
      const emptyKey = "";

      await enforceLoginRateLimitCore(firestore, emptyKey);

      const docRef = firestore
        .collection("admin-rate-limits")
        .doc("admin_login:unknown");
      const snapshot = await docRef.get();

      expect(snapshot.exists).toBe(true);
    });

    it("空白のみのキーは'unknown'に変換される", async () => {
      const whitespaceKey = "   ";

      await enforceLoginRateLimitCore(firestore, whitespaceKey);

      const docRef = firestore
        .collection("admin-rate-limits")
        .doc("admin_login:unknown");
      const snapshot = await docRef.get();

      expect(snapshot.exists).toBe(true);
    });
  });

  describe("異なるユーザーの独立性", () => {
    it("異なるユーザーのレート制限は独立している", async () => {
      const user1 = `user1-${Date.now()}@example.com`;
      const user2 = `user2-${Date.now()}@example.com`;

      // user1が5回試行
      for (let i = 0; i < 5; i++) {
        await enforceLoginRateLimitCore(firestore, user1);
      }

      // user1は制限される
      const user1Result = await enforceLoginRateLimitCore(firestore, user1);
      expect(user1Result.allowed).toBe(false);

      // user2はまだ制限されない
      const user2Result = await enforceLoginRateLimitCore(firestore, user2);
      expect(user2Result.allowed).toBe(true);
    });
  });

  describe("データ永続化の検証", () => {
    it("ログイン試行データがFirestoreに正しいプレフィックスで保存される", async () => {
      const key = `persist-test-${Date.now()}@example.com`;

      await enforceLoginRateLimitCore(firestore, key);

      const docRef = firestore
        .collection("admin-rate-limits")
        .doc(`admin_login:${key}`);
      const snapshot = await docRef.get();

      expect(snapshot.exists).toBe(true);
      const data = snapshot.data();
      expect(data?.count).toBe(1);
    });
  });

  describe("並列ログイン試行の処理", () => {
    it("並列のログイン試行が正しくカウントされる", async () => {
      const key = `parallel-${Date.now()}@example.com`;

      const results = await Promise.all([
        enforceLoginRateLimitCore(firestore, key),
        enforceLoginRateLimitCore(firestore, key),
        enforceLoginRateLimitCore(firestore, key),
      ]);

      results.forEach((result) => {
        expect(result.allowed).toBe(true);
      });

      const docRef = firestore
        .collection("admin-rate-limits")
        .doc(`admin_login:${key}`);
      const snapshot = await docRef.get();
      const data = snapshot.data();

      expect(data?.count).toBe(3);
    });
  });
});
