/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, screen, act } from "@testing-library/react";

const mockUsePathname = vi.fn(() => "/");

vi.mock("next/navigation", () => ({
  usePathname: () => mockUsePathname(),
}));

describe("components/molecules/navigation/NavigationProvider", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.useFakeTimers();
    mockUsePathname.mockReturnValue("/");
  });

  afterEach(() => {
    vi.useRealTimers();
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

  it("startNavigation 呼び出し後 200ms 経過するとプログレスバーが表示される", async () => {
    const { NavigationProvider, useNavigation } = await import(
      "@shared/components/molecules/navigation/provider"
    );

    const TestComponent = () => {
      const { startNavigation } = useNavigation();
      return (
        <button type="button" onClick={startNavigation}>
          ナビゲート
        </button>
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

    expect(
      screen.queryByTestId("top-progress-bar"),
    ).not.toBeInTheDocument();

    await act(async () => {
      vi.advanceTimersByTime(200);
    });

    expect(screen.getByTestId("top-progress-bar")).toBeInTheDocument();
  });

  it("startNavigation 呼び出し後 200ms 未満ではプログレスバーが表示されない", async () => {
    const { NavigationProvider, useNavigation } = await import(
      "@shared/components/molecules/navigation/provider"
    );

    const TestComponent = () => {
      const { startNavigation } = useNavigation();
      return (
        <button type="button" onClick={startNavigation}>
          ナビゲート
        </button>
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

    await act(async () => {
      vi.advanceTimersByTime(199);
    });

    expect(
      screen.queryByTestId("top-progress-bar"),
    ).not.toBeInTheDocument();
  });

  it("ルートが変化するとナビゲーションが完了状態になりプログレスバーが消える", async () => {
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

    await act(async () => {
      vi.advanceTimersByTime(200);
    });

    expect(screen.getByTestId("status").textContent).toBe("navigating");
    expect(screen.getByTestId("top-progress-bar")).toBeInTheDocument();

    mockUsePathname.mockReturnValue("/articles");

    await act(async () => {
      rerender(
        <NavigationProvider>
          <TestComponent />
        </NavigationProvider>,
      );
    });

    expect(screen.getByTestId("status").textContent).toBe("idle");
    expect(screen.getByTestId("top-progress-bar")).toBeInTheDocument();
    expect(
      screen.getByTestId("top-progress-bar").getAttribute("data-completing"),
    ).toBe("true");

    await act(async () => {
      vi.advanceTimersByTime(200);
    });

    expect(
      screen.queryByTestId("top-progress-bar"),
    ).not.toBeInTheDocument();
  });

  it("200ms 以内にルートが変化した場合はプログレスバーが表示されない（フラッシュ防止）", async () => {
    const { NavigationProvider, useNavigation } = await import(
      "@shared/components/molecules/navigation/provider"
    );

    const TestComponent = () => {
      const { startNavigation } = useNavigation();
      return (
        <button type="button" onClick={startNavigation}>
          ナビゲート
        </button>
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

    await act(async () => {
      vi.advanceTimersByTime(100);
    });

    mockUsePathname.mockReturnValue("/articles");

    await act(async () => {
      rerender(
        <NavigationProvider>
          <TestComponent />
        </NavigationProvider>,
      );
    });

    await act(async () => {
      vi.advanceTimersByTime(200);
    });

    expect(
      screen.queryByTestId("top-progress-bar"),
    ).not.toBeInTheDocument();
  });
});
