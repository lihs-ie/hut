/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Theme } from "@shared/domains/common/theme";

const mockCookies = {
  get: vi.fn(),
  set: vi.fn(),
};

vi.mock("next/headers", () => ({
  cookies: vi.fn(() => Promise.resolve(mockCookies)),
}));

vi.mock("next/cache", () => ({
  revalidatePath: vi.fn(),
}));

describe("theme actions - セキュリティ", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockCookies.get.mockReset();
    mockCookies.set.mockReset();
  });

  describe("toggleTheme - Cookie セキュリティ設定", () => {
    it("httpOnly: true でCookieを設定する", async () => {
      mockCookies.get.mockReturnValue({ value: Theme.LIGHT });

      const { toggleTheme } = await import("@shared/actions/theme");
      await toggleTheme();

      expect(mockCookies.set).toHaveBeenCalledWith(
        "theme",
        Theme.DARK,
        expect.objectContaining({ httpOnly: true }),
      );
    });

    it("NODE_ENV=production の場合 secure: true でCookieを設定する", async () => {
      const originalNodeEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = "production";
      mockCookies.get.mockReturnValue({ value: Theme.LIGHT });

      const { toggleTheme } = await import("@shared/actions/theme");
      await toggleTheme();

      expect(mockCookies.set).toHaveBeenCalledWith(
        "theme",
        Theme.DARK,
        expect.objectContaining({ secure: true }),
      );

      process.env.NODE_ENV = originalNodeEnv;
    });

    it("NODE_ENV=development の場合 secure: false でCookieを設定する", async () => {
      const originalNodeEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = "development";
      mockCookies.get.mockReturnValue({ value: Theme.LIGHT });

      const { toggleTheme } = await import("@shared/actions/theme");
      await toggleTheme();

      expect(mockCookies.set).toHaveBeenCalledWith(
        "theme",
        Theme.DARK,
        expect.objectContaining({ secure: false }),
      );

      process.env.NODE_ENV = originalNodeEnv;
    });
  });
});
