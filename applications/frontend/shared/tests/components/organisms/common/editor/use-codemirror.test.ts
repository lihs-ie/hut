/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { renderHook, act } from "@testing-library/react";
import { useCodeMirror } from "@shared/components/organisms/common/editor/use-codemirror";

describe("useCodeMirror", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("初期値でeditorViewがnullである", () => {
    const onChange = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange })
    );

    expect(result.current.containerRef).toBeDefined();
    expect(result.current.containerRef.current).toBeNull();
  });

  it("containerRefを返す", () => {
    const onChange = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "テスト", onChange })
    );

    expect(result.current.containerRef).toBeDefined();
  });

  it("insertTextメソッドを返す", () => {
    const onChange = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange })
    );

    expect(typeof result.current.insertText).toBe("function");
  });

  it("focusメソッドを返す", () => {
    const onChange = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange })
    );

    expect(typeof result.current.focus).toBe("function");
  });

  it("onSaveコールバックが設定可能である", () => {
    const onChange = vi.fn();
    const onSave = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange, onSave })
    );

    expect(result.current.containerRef).toBeDefined();
  });

  it("insertTextはエディタがない場合でもエラーを投げない", () => {
    const onChange = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange })
    );

    expect(() => {
      act(() => {
        result.current.insertText("テキスト");
      });
    }).not.toThrow();
  });

  it("focusはエディタがない場合でもエラーを投げない", () => {
    const onChange = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange })
    );

    expect(() => {
      act(() => {
        result.current.focus();
      });
    }).not.toThrow();
  });

  it("onPasteコールバックを受け取れる", () => {
    const onChange = vi.fn();
    const onPaste = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange, onPaste })
    );

    expect(result.current.containerRef).toBeDefined();
  });

  it("replaceTextメソッドを返す", () => {
    const onChange = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange })
    );

    expect(typeof result.current.replaceText).toBe("function");
  });

  it("replaceTextはエディタがない場合でもエラーを投げない", () => {
    const onChange = vi.fn();
    const { result } = renderHook(() =>
      useCodeMirror({ value: "", onChange })
    );

    expect(() => {
      act(() => {
        result.current.replaceText("before", "after");
      });
    }).not.toThrow();
  });
});
