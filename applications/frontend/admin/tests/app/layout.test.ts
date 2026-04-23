import { describe, it, expect, vi, beforeEach } from "vitest";

const notoSansJPMock = vi.fn(() => ({ variable: "--font-noto-sans-jp" }));
const interMock = vi.fn(() => ({ variable: "--font-inter" }));
const geistMonoMock = vi.fn(() => ({ variable: "--font-geist-mono" }));

vi.mock("next/font/google", () => ({
  Noto_Sans_JP: notoSansJPMock,
  Inter: interMock,
  Geist_Mono: geistMonoMock,
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

describe("admin font configuration", () => {
  beforeEach(() => {
    notoSansJPMock.mockClear();
    interMock.mockClear();
    geistMonoMock.mockClear();
    vi.resetModules();
  });

  it("Noto_Sans_JP は CSS で使用される weight 全て (400-900) で呼び出される", async () => {
    await import("../../src/app/layout");
    expect(notoSansJPMock).toHaveBeenCalledTimes(1);
    const callArgs = notoSansJPMock.mock.calls[0][0] as {
      weight: string[];
    };
    expect(callArgs.weight).toEqual([
      "400",
      "500",
      "600",
      "700",
      "800",
      "900",
    ]);
  });

  it("Noto_Sans_JP には display: 'swap' が指定される", async () => {
    await import("../../src/app/layout");
    const callArgs = notoSansJPMock.mock.calls[0][0] as {
      display: string;
    };
    expect(callArgs.display).toBe("swap");
  });

  it("Geist_Mono には display: 'swap' と preload: false が指定される", async () => {
    await import("../../src/app/layout");
    expect(geistMonoMock).toHaveBeenCalledTimes(1);
    const callArgs = geistMonoMock.mock.calls[0][0] as {
      display: string;
      preload: boolean;
    };
    expect(callArgs.display).toBe("swap");
    expect(callArgs.preload).toBe(false);
  });

  it("Inter は呼び出されない", async () => {
    await import("../../src/app/layout");
    expect(interMock).toHaveBeenCalledTimes(0);
  });
});
