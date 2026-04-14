import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { createBaseNextConfig } from "../../next.config.shared";

describe("next.config.shared", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    process.env = { ...originalEnv };
    delete process.env.IMAGE_REMOTE_PATTERNS;
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  describe("createBaseNextConfig", () => {
    it("デフォルトでは dangerouslyAllowLocalIP が images に含まれない", () => {
      const config = createBaseNextConfig();

      expect(config.images).toBeDefined();
      expect(config.images).not.toHaveProperty("dangerouslyAllowLocalIP");
    });

    it("useFirebaseEmulator: true を渡すと dangerouslyAllowLocalIP とエミュレーターパターンが反映される", () => {
      const config = createBaseNextConfig({
        useFirebaseEmulator: true,
      });

      expect(config.images?.dangerouslyAllowLocalIP).toBe(true);
      expect(config.images?.remotePatterns).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            protocol: "http",
            hostname: "localhost",
          }),
        ]),
      );
    });

    it("IMAGE_REMOTE_PATTERNS 環境変数からパースしたパターンが remotePatterns に含まれる", () => {
      process.env.IMAGE_REMOTE_PATTERNS =
        "https://firebasestorage.googleapis.com";

      const config = createBaseNextConfig();

      expect(config.images?.remotePatterns).toEqual([
        expect.objectContaining({
          protocol: "https",
          hostname: "firebasestorage.googleapis.com",
        }),
      ]);
    });

    it("IMAGE_REMOTE_PATTERNS とエミュレーターパターンを同時に使用できる", () => {
      process.env.IMAGE_REMOTE_PATTERNS =
        "https://firebasestorage.googleapis.com";

      const config = createBaseNextConfig({
        useFirebaseEmulator: true,
      });

      expect(config.images?.dangerouslyAllowLocalIP).toBe(true);
      expect(config.images?.remotePatterns).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            protocol: "https",
            hostname: "firebasestorage.googleapis.com",
          }),
          expect.objectContaining({
            protocol: "http",
            hostname: "localhost",
          }),
        ]),
      );
    });

    it("IMAGE_REMOTE_PATTERNS が未設定かつエミュレーター無効の場合、remotePatterns は空配列", () => {
      const config = createBaseNextConfig();

      expect(config.images?.remotePatterns).toEqual([]);
    });

    it("targetRuntime を省略すると node を選択し output=standalone を維持する", () => {
      const config = createBaseNextConfig();

      expect(config.output).toBe("standalone");
      expect(config.images).not.toHaveProperty("unoptimized");
    });

    it("targetRuntime: 'edge-worker' を渡すと images.unoptimized が true になり output は省略される", () => {
      const config = createBaseNextConfig({ targetRuntime: "edge-worker" });

      expect(config.output).toBeUndefined();
      expect(config.images?.unoptimized).toBe(true);
    });

    it("targetRuntime: 'node' を明示しても従来通り standalone 出力になる", () => {
      const config = createBaseNextConfig({ targetRuntime: "node" });

      expect(config.output).toBe("standalone");
    });
  });
});
