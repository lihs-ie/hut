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

describe("theme actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockCookies.get.mockReset();
    mockCookies.set.mockReset();
  });

  describe("currentTheme", () => {
    it("cookieからテーマを取得できる（light）", async () => {
      mockCookies.get.mockReturnValue({ value: Theme.LIGHT });

      const { currentTheme } = await import("@shared/actions/theme");
      const result = await currentTheme();

      expect(result).toBe(Theme.LIGHT);
      expect(mockCookies.get).toHaveBeenCalledWith("theme");
    });

    it("cookieからテーマを取得できる（dark）", async () => {
      mockCookies.get.mockReturnValue({ value: Theme.DARK });

      const { currentTheme } = await import("@shared/actions/theme");
      const result = await currentTheme();

      expect(result).toBe(Theme.DARK);
      expect(mockCookies.get).toHaveBeenCalledWith("theme");
    });

    it("cookieにテーマがない場合はデフォルトでlightを返す", async () => {
      mockCookies.get.mockReturnValue(undefined);

      const { currentTheme } = await import("@shared/actions/theme");
      const result = await currentTheme();

      expect(result).toBe(Theme.LIGHT);
    });
  });

  describe("toggleTheme", () => {
    const expectedCookieOptions = { path: "/", sameSite: "lax" };

    it("lightからdarkに切り替えできる", async () => {
      mockCookies.get.mockReturnValue({ value: Theme.LIGHT });

      const { toggleTheme } = await import("@shared/actions/theme");
      await toggleTheme();

      expect(mockCookies.set).toHaveBeenCalledWith("theme", Theme.DARK, expectedCookieOptions);
    });

    it("darkからlightに切り替えできる", async () => {
      mockCookies.get.mockReturnValue({ value: Theme.DARK });

      const { toggleTheme } = await import("@shared/actions/theme");
      await toggleTheme();

      expect(mockCookies.set).toHaveBeenCalledWith("theme", Theme.LIGHT, expectedCookieOptions);
    });

    it("cookieにテーマがない場合はlightからdarkに切り替える", async () => {
      mockCookies.get.mockReturnValue(undefined);

      const { toggleTheme } = await import("@shared/actions/theme");
      await toggleTheme();

      expect(mockCookies.set).toHaveBeenCalledWith("theme", Theme.DARK, expectedCookieOptions);
    });

    it("テーマ切り替え後にrevalidatePathが呼ばれる", async () => {
      mockCookies.get.mockReturnValue({ value: Theme.LIGHT });

      const { revalidatePath } = await import("next/cache");
      const { toggleTheme } = await import("@shared/actions/theme");
      await toggleTheme();

      expect(revalidatePath).toHaveBeenCalledWith("/", "layout");
    });
  });
});
