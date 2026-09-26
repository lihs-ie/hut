import { describe, it, expect } from "vitest";
import { createBaseNextConfig } from "../../next.config.shared";

describe("next.config.shared - 最適化設定", () => {
  describe("experimental.optimizePackageImports", () => {
    it("@hut/shared が optimizePackageImports に含まれる", () => {
      const config = createBaseNextConfig({ useFirebaseEmulator: false });

      const optimizePackageImports =
        config.experimental?.optimizePackageImports;

      expect(Array.isArray(optimizePackageImports)).toBe(true);
      expect(optimizePackageImports).toContain("@hut/shared");
    });

    it("nuqs が optimizePackageImports に含まれる", () => {
      const config = createBaseNextConfig({ useFirebaseEmulator: false });

      expect(config.experimental?.optimizePackageImports).toContain("nuqs");
    });
  });

  describe("images.formats", () => {
    it("avif と webp が images.formats に含まれる", () => {
      const config = createBaseNextConfig({ useFirebaseEmulator: false });

      expect(config.images?.formats).toEqual(
        expect.arrayContaining(["image/avif", "image/webp"])
      );
    });
  });

  describe("images.minimumCacheTTL", () => {
    it("minimumCacheTTL が 86400 に設定されている", () => {
      const config = createBaseNextConfig({ useFirebaseEmulator: false });

      expect(config.images?.minimumCacheTTL).toBe(86400);
    });
  });

  describe("既存設定の維持", () => {
    it("output が standalone のまま", () => {
      const config = createBaseNextConfig({ useFirebaseEmulator: false });

      expect(config.output).toBe("standalone");
    });

    it("reactCompiler が true のまま", () => {
      const config = createBaseNextConfig({ useFirebaseEmulator: false });

      expect(config.reactCompiler).toBe(true);
    });

    it("transpilePackages に @hut/shared が含まれる", () => {
      const config = createBaseNextConfig({ useFirebaseEmulator: false });

      expect(config.transpilePackages).toContain("@hut/shared");
    });
  });
});
