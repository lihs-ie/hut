/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { renderHook, act } from "@testing-library/react";
import {
  useDebounce,
  useDebouncedCallback,
} from "@shared/components/global/hooks/use-debounce";

const DEFAULT_DELAY = 300;

function advanceTimer(milliseconds: number): void {
  act(() => {
    vi.advanceTimersByTime(milliseconds);
  });
}

describe("hooks/useDebounce", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
    vi.clearAllMocks();
  });

  describe("useDebounce", () => {
    it("初期値が即座に返される", () => {
      const { result } = renderHook(() => useDebounce("initial"));
      expect(result.current).toBe("initial");
    });

    it("値の更新がデフォルト遅延後に反映される", () => {
      const { result, rerender } = renderHook(
        ({ value }) => useDebounce(value),
        { initialProps: { value: "initial" } }
      );

      rerender({ value: "updated" });
      expect(result.current).toBe("initial");

      advanceTimer(DEFAULT_DELAY);
      expect(result.current).toBe("updated");
    });

    it("カスタム遅延時間が適用される", () => {
      const customDelay = 500;
      const { result, rerender } = renderHook(
        ({ value, delay }) => useDebounce(value, delay),
        { initialProps: { value: "initial", delay: customDelay } }
      );

      rerender({ value: "updated", delay: customDelay });
      advanceTimer(DEFAULT_DELAY);
      expect(result.current).toBe("initial");

      advanceTimer(200);
      expect(result.current).toBe("updated");
    });

    it("連続した値の変更では最後の値のみが反映される", () => {
      const { result, rerender } = renderHook(
        ({ value }) => useDebounce(value),
        { initialProps: { value: "first" } }
      );

      rerender({ value: "second" });
      advanceTimer(100);
      rerender({ value: "third" });
      advanceTimer(100);
      rerender({ value: "fourth" });
      expect(result.current).toBe("first");

      advanceTimer(DEFAULT_DELAY);
      expect(result.current).toBe("fourth");
    });

    it("数値型の値が正しくデバウンスされる", () => {
      const { result, rerender } = renderHook(
        ({ value }) => useDebounce(value),
        { initialProps: { value: 0 } }
      );

      rerender({ value: 42 });
      expect(result.current).toBe(0);

      advanceTimer(DEFAULT_DELAY);
      expect(result.current).toBe(42);
    });

    it("オブジェクト型の値が正しくデバウンスされる", () => {
      const initial = { name: "initial" };
      const updated = { name: "updated" };

      const { result, rerender } = renderHook(
        ({ value }) => useDebounce(value),
        { initialProps: { value: initial } }
      );

      rerender({ value: updated });
      expect(result.current).toBe(initial);

      advanceTimer(DEFAULT_DELAY);
      expect(result.current).toBe(updated);
    });

    it("配列型の値が正しくデバウンスされる", () => {
      const initial = [1, 2, 3];
      const updated = [4, 5, 6];

      const { result, rerender } = renderHook(
        ({ value }) => useDebounce(value),
        { initialProps: { value: initial } }
      );

      rerender({ value: updated });
      advanceTimer(DEFAULT_DELAY);
      expect(result.current).toBe(updated);
    });

    it("null と undefined が正しくデバウンスされる", () => {
      const { result, rerender } = renderHook(
        ({ value }) => useDebounce(value),
        { initialProps: { value: "initial" as string | null | undefined } }
      );

      rerender({ value: null });
      advanceTimer(DEFAULT_DELAY);
      expect(result.current).toBeNull();

      rerender({ value: undefined });
      advanceTimer(DEFAULT_DELAY);
      expect(result.current).toBeUndefined();
    });

    it("遅延が0の場合も正しく動作する", () => {
      const { result, rerender } = renderHook(
        ({ value }) => useDebounce(value, 0),
        { initialProps: { value: "initial" } }
      );

      rerender({ value: "updated" });
      advanceTimer(0);
      expect(result.current).toBe("updated");
    });

    it("アンマウント時にタイマーがクリアされる", () => {
      const clearTimeoutSpy = vi.spyOn(global, "clearTimeout");

      const { unmount, rerender } = renderHook(
        ({ value }) => useDebounce(value),
        { initialProps: { value: "initial" } }
      );

      rerender({ value: "updated" });
      unmount();

      expect(clearTimeoutSpy).toHaveBeenCalled();
    });
  });

  describe("useDebouncedCallback", () => {
    it("コールバックがデフォルト遅延後に呼ばれる", () => {
      const callback = vi.fn();
      const { result } = renderHook(() => useDebouncedCallback(callback));

      act(() => {
        result.current();
      });
      expect(callback).not.toHaveBeenCalled();

      advanceTimer(DEFAULT_DELAY);
      expect(callback).toHaveBeenCalledTimes(1);
    });

    it("引数が正しくコールバックに渡される", () => {
      const callback = vi.fn();
      const { result } = renderHook(() => useDebouncedCallback(callback));

      act(() => {
        result.current("arg1", 123, { key: "value" });
      });
      advanceTimer(DEFAULT_DELAY);

      expect(callback).toHaveBeenCalledWith("arg1", 123, { key: "value" });
    });

    it("カスタム遅延時間が適用される", () => {
      const customDelay = 500;
      const callback = vi.fn();
      const { result } = renderHook(() =>
        useDebouncedCallback(callback, customDelay)
      );

      act(() => {
        result.current();
      });

      advanceTimer(DEFAULT_DELAY);
      expect(callback).not.toHaveBeenCalled();

      advanceTimer(200);
      expect(callback).toHaveBeenCalledTimes(1);
    });

    it("連続呼び出しでは最後の呼び出しのみが実行される", () => {
      const callback = vi.fn();
      const { result } = renderHook(() => useDebouncedCallback(callback));

      act(() => {
        result.current("first");
      });
      advanceTimer(100);
      act(() => {
        result.current("second");
      });
      advanceTimer(100);
      act(() => {
        result.current("third");
      });
      advanceTimer(DEFAULT_DELAY);

      expect(callback).toHaveBeenCalledTimes(1);
      expect(callback).toHaveBeenCalledWith("third");
    });

    it("遅延後に再度呼び出し可能", () => {
      const callback = vi.fn();
      const { result } = renderHook(() => useDebouncedCallback(callback));

      act(() => {
        result.current("first");
      });
      advanceTimer(DEFAULT_DELAY);
      expect(callback).toHaveBeenCalledTimes(1);
      expect(callback).toHaveBeenLastCalledWith("first");

      act(() => {
        result.current("second");
      });
      advanceTimer(DEFAULT_DELAY);
      expect(callback).toHaveBeenCalledTimes(2);
      expect(callback).toHaveBeenLastCalledWith("second");
    });

    it("アンマウント時にタイマーがクリアされる", () => {
      const callback = vi.fn();
      const clearTimeoutSpy = vi.spyOn(global, "clearTimeout");

      const { result, unmount } = renderHook(() =>
        useDebouncedCallback(callback)
      );

      act(() => {
        result.current();
      });
      unmount();

      expect(clearTimeoutSpy).toHaveBeenCalled();
      advanceTimer(DEFAULT_DELAY);
      expect(callback).not.toHaveBeenCalled();
    });

    it("コールバックの参照が変わっても新しいコールバックが使用される", () => {
      const firstCallback = vi.fn();
      const secondCallback = vi.fn();

      const { result, rerender } = renderHook(
        ({ callback }) => useDebouncedCallback(callback),
        { initialProps: { callback: firstCallback } }
      );

      act(() => {
        result.current();
      });
      rerender({ callback: secondCallback });
      advanceTimer(DEFAULT_DELAY);

      expect(firstCallback).toHaveBeenCalledTimes(1);
      expect(secondCallback).not.toHaveBeenCalled();
    });

    it("遅延時間が変更されても正しく動作する", () => {
      const callback = vi.fn();

      const { result, rerender } = renderHook(
        ({ delay }) => useDebouncedCallback(callback, delay),
        { initialProps: { delay: DEFAULT_DELAY } }
      );

      rerender({ delay: 500 });
      act(() => {
        result.current();
      });

      advanceTimer(DEFAULT_DELAY);
      expect(callback).not.toHaveBeenCalled();

      advanceTimer(200);
      expect(callback).toHaveBeenCalledTimes(1);
    });

    it("複数の引数を持つコールバックが正しく動作する", () => {
      const callback = vi.fn(
        (name: string, age: number, options: { active: boolean }) => {
          return { name, age, options };
        }
      );

      const { result } = renderHook(() => useDebouncedCallback(callback));

      act(() => {
        result.current("John", 30, { active: true });
      });
      advanceTimer(DEFAULT_DELAY);

      expect(callback).toHaveBeenCalledWith("John", 30, { active: true });
    });
  });
});
