/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, act } from "@testing-library/react";

const mockUsePathname = vi.fn(() => "/");

vi.mock("next/navigation", () => ({
  usePathname: () => mockUsePathname(),
}));

describe("components/molecules/navigation/NavigationProvider", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockUsePathname.mockReturnValue("/");
  });

  it("NavigationProvider 配下でコンテンツが表示される", async () => {
    const { NavigationProvider } = await import(
      "@shared/components/molecules/navigation/provider"
    );

    render(
      <NavigationProvider>
        <div>コンテンツ</div>
      </NavigationProvider>,
    );

    expect(screen.getByText("コンテンツ")).toBeInTheDocument();
  });

  it("ルートが変化するとナビゲーション状態が更新される", async () => {
    const { NavigationProvider } = await import(
      "@shared/components/molecules/navigation/provider"
    );

    const { rerender } = render(
      <NavigationProvider>
        <div>コンテンツ</div>
      </NavigationProvider>,
    );

    mockUsePathname.mockReturnValue("/articles");

    await act(async () => {
      rerender(
        <NavigationProvider>
          <div>コンテンツ</div>
        </NavigationProvider>,
      );
    });

    expect(screen.getByText("コンテンツ")).toBeInTheDocument();
  });

  it("ページ遷移中は LoadingOverlay が表示される", async () => {
    const { NavigationProvider, useNavigation } = await import(
      "@shared/components/molecules/navigation/provider"
    );

    const TestComponent = () => {
      const { isNavigating, startNavigation } = useNavigation();
      return (
        <>
          <button type="button" onClick={startNavigation}>
            ナビゲート
          </button>
          {isNavigating && <div data-testid="loading">ローディング中</div>}
        </>
      );
    };

    render(
      <NavigationProvider>
        <TestComponent />
      </NavigationProvider>,
    );

    await act(async () => {
      screen.getByText("ナビゲート").click();
    });

    expect(screen.getByTestId("loading")).toBeInTheDocument();
  });

  it("ルートが変化するとナビゲーションが完了状態になる", async () => {
    const { NavigationProvider, useNavigation } = await import(
      "@shared/components/molecules/navigation/provider"
    );

    const TestComponent = () => {
      const { isNavigating, startNavigation } = useNavigation();
      return (
        <>
          <button type="button" onClick={startNavigation}>
            ナビゲート
          </button>
          <div data-testid="status">
            {isNavigating ? "navigating" : "idle"}
          </div>
        </>
      );
    };

    const { rerender } = render(
      <NavigationProvider>
        <TestComponent />
      </NavigationProvider>,
    );

    await act(async () => {
      screen.getByText("ナビゲート").click();
    });

    expect(screen.getByTestId("status").textContent).toBe("navigating");

    mockUsePathname.mockReturnValue("/articles");

    await act(async () => {
      rerender(
        <NavigationProvider>
          <TestComponent />
        </NavigationProvider>,
      );
    });

    expect(screen.getByTestId("status").textContent).toBe("idle");
  });
});
