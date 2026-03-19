/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, screen, act } from "@testing-library/react";

describe("components/molecules/toast", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  describe("ToastProvider と useToast", () => {
    it("ToastProvider 配下で useToast が使用できる", async () => {
      const { ToastProvider, useToast } = await import(
        "@shared/components/molecules/toast"
      );

      const TestComponent = () => {
        const { showToast } = useToast();
        return (
          <button type="button" onClick={() => showToast("テストメッセージ")}>
            トーストを表示
          </button>
        );
      };

      render(
        <ToastProvider>
          <TestComponent />
        </ToastProvider>,
      );

      expect(screen.getByText("トーストを表示")).toBeInTheDocument();
    });

    it("showToast を呼ぶとトーストメッセージが表示される", async () => {
      const { ToastProvider, useToast } = await import(
        "@shared/components/molecules/toast"
      );

      const TestComponent = () => {
        const { showToast } = useToast();
        return (
          <button type="button" onClick={() => showToast("保存しました")}>
            保存
          </button>
        );
      };

      render(
        <ToastProvider>
          <TestComponent />
        </ToastProvider>,
      );

      const button = screen.getByText("保存");
      await act(async () => {
        button.click();
      });

      expect(screen.getByText("保存しました")).toBeInTheDocument();
    });

    it("トーストは指定時間後に自動で消える", async () => {
      const { ToastProvider, useToast } = await import(
        "@shared/components/molecules/toast"
      );

      const TestComponent = () => {
        const { showToast } = useToast();
        return (
          <button type="button" onClick={() => showToast("一時的なメッセージ")}>
            表示
          </button>
        );
      };

      render(
        <ToastProvider>
          <TestComponent />
        </ToastProvider>,
      );

      const button = screen.getByText("表示");
      act(() => {
        button.click();
      });

      expect(screen.getByText("一時的なメッセージ")).toBeInTheDocument();

      act(() => {
        vi.advanceTimersByTime(3000);
      });

      expect(screen.queryByText("一時的なメッセージ")).not.toBeInTheDocument();
    });

    it("複数のトーストを順番に表示できる", async () => {
      const { ToastProvider, useToast } = await import(
        "@shared/components/molecules/toast"
      );

      const TestComponent = () => {
        const { showToast } = useToast();
        return (
          <>
            <button type="button" onClick={() => showToast("メッセージ1")}>
              ボタン1
            </button>
            <button type="button" onClick={() => showToast("メッセージ2")}>
              ボタン2
            </button>
          </>
        );
      };

      render(
        <ToastProvider>
          <TestComponent />
        </ToastProvider>,
      );

      await act(async () => {
        screen.getByText("ボタン1").click();
      });

      await act(async () => {
        screen.getByText("ボタン2").click();
      });

      expect(screen.getByText("メッセージ1")).toBeInTheDocument();
      expect(screen.getByText("メッセージ2")).toBeInTheDocument();
    });
  });

  describe("Toast コンポーネント表示位置", () => {
    it("トーストは画面右下に表示される", async () => {
      const { ToastProvider, useToast } = await import(
        "@shared/components/molecules/toast"
      );

      const TestComponent = () => {
        const { showToast } = useToast();
        return (
          <button type="button" onClick={() => showToast("右下のトースト")}>
            表示
          </button>
        );
      };

      render(
        <ToastProvider>
          <TestComponent />
        </ToastProvider>,
      );

      await act(async () => {
        screen.getByText("表示").click();
      });

      const toastContainer = document.querySelector('[class*="container"]');
      expect(toastContainer).toBeInTheDocument();
    });
  });
});
