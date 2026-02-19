import { describe, it, expect } from "vitest";
import { createBaseNextConfig } from "../../next.config.shared";

describe("next.config.shared", () => {
  describe("createBaseNextConfig", () => {
    it("デフォルトでは dangerouslyAllowLocalIP が images に含まれない", () => {
      const config = createBaseNextConfig();

      expect(config.images).toBeDefined();
      expect(config.images).not.toHaveProperty("dangerouslyAllowLocalIP");
    });

    it("dangerouslyAllowLocalIP: true を渡すと images に反映される", () => {
      const config = createBaseNextConfig({
        dangerouslyAllowLocalIP: true,
      });

      expect(config.images).toBeDefined();
      expect(config.images?.dangerouslyAllowLocalIP).toBe(true);
    });

    it("dangerouslyAllowLocalIP: false を渡すと images に反映される", () => {
      const config = createBaseNextConfig({
        dangerouslyAllowLocalIP: false,
      });

      expect(config.images).toBeDefined();
      expect(config.images?.dangerouslyAllowLocalIP).toBe(false);
    });

    it("additionalRemotePatterns と dangerouslyAllowLocalIP を同時に渡した場合、両方正しく反映される", () => {
      const config = createBaseNextConfig({
        additionalRemotePatterns: [
          {
            protocol: "http",
            hostname: "localhost",
          },
        ],
        dangerouslyAllowLocalIP: true,
      });

      expect(config.images).toBeDefined();
      expect(config.images?.dangerouslyAllowLocalIP).toBe(true);

      expect(config.images?.remotePatterns).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            protocol: "https",
            hostname: "**",
          }),
          expect.objectContaining({
            protocol: "http",
            hostname: "localhost",
          }),
        ]),
      );
    });
  });
});
