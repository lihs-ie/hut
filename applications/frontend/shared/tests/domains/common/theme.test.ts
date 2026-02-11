import { describe, it, expect } from "vitest";
import { Theme } from "@shared/domains/common/theme";

describe("domains/common/theme", () => {
  describe("Theme定数", () => {
    it("LIGHTは'light'", () => {
      expect(Theme.LIGHT).toBe("light");
    });

    it("DARKは'dark'", () => {
      expect(Theme.DARK).toBe("dark");
    });

    it("LIGHTとDARKのみが存在する", () => {
      const keys = Object.keys(Theme);
      expect(keys).toHaveLength(2);
      expect(keys).toContain("LIGHT");
      expect(keys).toContain("DARK");
    });

    it("値は'light'と'dark'のみ", () => {
      const values = Object.values(Theme);
      expect(values).toHaveLength(2);
      expect(values).toContain("light");
      expect(values).toContain("dark");
    });
  });
});
