/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";

const notoSansJPMock = vi.fn(() => ({ variable: "--font-noto-sans-jp" }));
const interMock = vi.fn(() => ({ variable: "--font-inter" }));
const geistMonoMock = vi.fn(() => ({ variable: "--font-geist-mono" }));

vi.mock("next/font/google", () => ({
  Noto_Sans_JP: notoSansJPMock,
  Inter: interMock,
  Geist_Mono: geistMonoMock,
}));

vi.mock("@shared/global.css", () => ({}));
vi.mock("@shared/components/organisms/header", () => ({ Header: vi.fn() }));
vi.mock("@shared/components/organisms/footer", () => ({ Footer: vi.fn() }));
vi.mock("@shared/components/organisms/footer/index.presenter", () => ({ FooterPresenter: vi.fn() }));
vi.mock("@shared/components/molecules/boundary/footer-error", () => ({ FooterErrorBoundary: vi.fn() }));
vi.mock("@shared/actions/admin", () => ({ getProfile: vi.fn() }));
vi.mock("@shared/components/molecules/navigation/provider", () => ({ NavigationProvider: vi.fn() }));
vi.mock("@shared/components/molecules/toast", () => ({ ToastProvider: vi.fn() }));
vi.mock("@shared/components/molecules/theme/provider", () => ({ ThemeProvider: vi.fn() }));
vi.mock("nuqs/adapters/next/app", () => ({ NuqsAdapter: vi.fn() }));

describe("layout metadata - robots", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    process.env = { ...originalEnv };
    vi.resetModules();
  });

  afterEach(() => {
    process.env = originalEnv;
    vi.unstubAllEnvs();
  });

  it("DISALLOW_ROBOTS が未設定の場合は index: true, follow: true", async () => {
    delete process.env.DISALLOW_ROBOTS;
    const { metadata } = await import("../../src/app/layout");
    const robots = metadata.robots as { index: boolean; follow: boolean };
    expect(robots.index).toBe(true);
    expect(robots.follow).toBe(true);
  });

  it("DISALLOW_ROBOTS が 'true' の場合は index: false, follow: false", async () => {
    process.env.DISALLOW_ROBOTS = "true";
    const { metadata } = await import("../../src/app/layout");
    const robots = metadata.robots as { index: boolean; follow: boolean };
    expect(robots.index).toBe(false);
    expect(robots.follow).toBe(false);
  });

  it("DISALLOW_ROBOTS が 'false' の場合は index: true, follow: true", async () => {
    process.env.DISALLOW_ROBOTS = "false";
    const { metadata } = await import("../../src/app/layout");
    const robots = metadata.robots as { index: boolean; follow: boolean };
    expect(robots.index).toBe(true);
    expect(robots.follow).toBe(true);
  });
});

describe("RootLayout", () => {
  beforeEach(() => {
    vi.resetModules();
  });

  it("html 要素をルートとして返す", async () => {
    const { default: RootLayout } = await import("../../src/app/layout");

    const element = RootLayout({ children: null });

    expect(element.type).toBe("html");
    expect(element.props.lang).toBe("ja");
  });
});

describe("font configuration", () => {
  beforeEach(() => {
    notoSansJPMock.mockClear();
    interMock.mockClear();
    geistMonoMock.mockClear();
    vi.resetModules();
  });

  it("Noto_Sans_JP は weight ['400','500','700','800','900'] で呼び出される", async () => {
    await import("../../src/app/layout");
    expect(notoSansJPMock).toHaveBeenCalledTimes(1);
    const callArgs = notoSansJPMock.mock.calls[0][0] as {
      weight: string[];
    };
    expect(callArgs.weight).toEqual(["400", "500", "700", "800", "900"]);
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
