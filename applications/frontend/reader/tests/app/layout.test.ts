/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";

vi.mock("next/font/google", () => ({
  Noto_Sans_JP: vi.fn(() => ({ variable: "--font-noto-sans-jp" })),
  Inter: vi.fn(() => ({ variable: "--font-inter" })),
  Geist_Mono: vi.fn(() => ({ variable: "--font-geist-mono" })),
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
