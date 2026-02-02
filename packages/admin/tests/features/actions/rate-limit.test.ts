/**
 * Rate Limit Actions Feature Test
 *
 * Firebase Emulatorを使用してレート制限アクションの統合テストを行います。
 */

import { describe, it, expect, beforeEach, afterAll } from "vitest";
import {
  createAdminFeatureTestContext,
  cleanupAdminFeatureTest,
  getTestFirestore,
} from "../setup";
import { enforceRateLimit, type RateLimitConfig } from "@/aspects/rate-limit";
import { isResourceExhaustedError } from "@shared/aspects/error";
import type { Firestore } from "firebase-admin/firestore";

describe("Feature: Rate Limit Actions (実DB接続)", () => {
  let firestore: Firestore;

  beforeEach(async () => {
    await createAdminFeatureTestContext();
    firestore = getTestFirestore();
  });

  afterAll(async () => {
    await cleanupAdminFeatureTest();
  });

  describe("enforceRateLimit", () => {
    const createConfig = (
      key: string,
      limit = 5,
      windowMs = 60000
    ): RateLimitConfig => ({
      key,
      limit,
      windowMs,
    });

    describe("新規リクエストの処理", () => {
      it("最初のリクエストは許可される", async () => {
        const config = createConfig(`test-new-${Date.now()}`);

        const result = await enforceRateLimit(firestore, config).unwrap();

        expect(result).toBeUndefined();
      });

      it("リミット内の複数リクエストは許可される", async () => {
        const config = createConfig(`test-multiple-${Date.now()}`, 3);

        await enforceRateLimit(firestore, config).unwrap();
        await enforceRateLimit(firestore, config).unwrap();
        const result = await enforceRateLimit(firestore, config).unwrap();

        expect(result).toBeUndefined();
      });
    });

    describe("リミット超過の処理", () => {
      it("リミットを超えるとResourceExhaustedErrorが返る", async () => {
        const config = createConfig(`test-exceed-${Date.now()}`, 2);

        await enforceRateLimit(firestore, config).unwrap();
        await enforceRateLimit(firestore, config).unwrap();
        const error = await enforceRateLimit(firestore, config).unwrapError();

        expect(isResourceExhaustedError(error)).toBe(true);
        expect(error.message).toBe(
          "ログイン試行が多すぎます。しばらくしてから再試行してください。"
        );
      });

      it("リミット超過後も拒否が継続する", async () => {
        const config = createConfig(`test-continuous-${Date.now()}`, 1);

        await enforceRateLimit(firestore, config).unwrap();

        const error1 = await enforceRateLimit(firestore, config).unwrapError();
        const error2 = await enforceRateLimit(firestore, config).unwrapError();

        expect(isResourceExhaustedError(error1)).toBe(true);
        expect(isResourceExhaustedError(error2)).toBe(true);
      });
    });

    describe("ウィンドウのリセット", () => {
      it("異なるキーは独立してカウントされる", async () => {
        const config1 = createConfig(`test-key-a-${Date.now()}`, 1);
        const config2 = createConfig(`test-key-b-${Date.now()}`, 1);

        await enforceRateLimit(firestore, config1).unwrap();
        await enforceRateLimit(firestore, config2).unwrap();

        const error1 = await enforceRateLimit(firestore, config1).unwrapError();
        const error2 = await enforceRateLimit(firestore, config2).unwrapError();

        expect(isResourceExhaustedError(error1)).toBe(true);
        expect(isResourceExhaustedError(error2)).toBe(true);
      });
    });

    describe("データ永続化の検証", () => {
      it("レート制限データがFirestoreに正しく保存される", async () => {
        const key = `test-persist-${Date.now()}`;
        const config = createConfig(key, 5);

        await enforceRateLimit(firestore, config).unwrap();

        const docRef = firestore
          .collection("admin-rate-limits")
          .doc(key);
        const snapshot = await docRef.get();

        expect(snapshot.exists).toBe(true);
        const data = snapshot.data();
        expect(data).toBeDefined();
        expect(data?.count).toBe(1);
        expect(data?.windowStart).toBeDefined();
      });

      it("連続リクエストでカウントが正しく増加する", async () => {
        const key = `test-increment-${Date.now()}`;
        const config = createConfig(key, 10);

        await enforceRateLimit(firestore, config).unwrap();
        await enforceRateLimit(firestore, config).unwrap();
        await enforceRateLimit(firestore, config).unwrap();

        const docRef = firestore
          .collection("admin-rate-limits")
          .doc(key);
        const snapshot = await docRef.get();
        const data = snapshot.data();

        expect(data?.count).toBe(3);
      });
    });

    describe("境界値テスト", () => {
      it("リミットが1の場合、最初のリクエストは許可される", async () => {
        const config = createConfig(`test-limit-1-${Date.now()}`, 1);

        const result = await enforceRateLimit(firestore, config).unwrap();

        expect(result).toBeUndefined();
      });

      it("リミットちょうどのリクエストまで許可される", async () => {
        const config = createConfig(`test-exact-limit-${Date.now()}`, 3);

        const result1 = await enforceRateLimit(firestore, config).unwrap();
        const result2 = await enforceRateLimit(firestore, config).unwrap();
        const result3 = await enforceRateLimit(firestore, config).unwrap();

        expect(result1).toBeUndefined();
        expect(result2).toBeUndefined();
        expect(result3).toBeUndefined();

        const error = await enforceRateLimit(firestore, config).unwrapError();
        expect(isResourceExhaustedError(error)).toBe(true);
      });
    });

    describe("並列リクエストの処理", () => {
      it("並列リクエストが正しくカウントされる", async () => {
        const config = createConfig(`test-parallel-${Date.now()}`, 10);

        const results = await Promise.all([
          enforceRateLimit(firestore, config).unwrap(),
          enforceRateLimit(firestore, config).unwrap(),
          enforceRateLimit(firestore, config).unwrap(),
        ]);

        results.forEach((result) => {
          expect(result).toBeUndefined();
        });

        const docRef = firestore
          .collection("admin-rate-limits")
          .doc(config.key);
        const snapshot = await docRef.get();
        const data = snapshot.data();

        expect(data?.count).toBe(3);
      });
    });
  });
});
