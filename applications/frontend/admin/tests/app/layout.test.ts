import { describe, it, expect, vi } from "vitest";

vi.mock("next/font/google", () => ({
  Noto_Sans_JP: () => ({ variable: "--font-noto-sans-jp" }),
  Inter: () => ({ variable: "--font-inter" }),
  Geist_Mono: () => ({ variable: "--font-geist-mono" }),
}));

vi.mock("../../src/actions/auth", () => ({
  isAdmin: vi.fn(),
  logout: vi.fn(),
}));

vi.mock("@shared/actions/admin", () => ({
  getProfile: vi.fn(),
}));

describe("admin root layout", () => {
  it("dynamic が force-dynamic に設定されている", async () => {
    const layoutModule = await import("../../src/app/layout");
    expect(layoutModule.dynamic).toBe("force-dynamic");
  });
});
