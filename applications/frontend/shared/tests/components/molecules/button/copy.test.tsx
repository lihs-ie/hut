/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, fireEvent, act, waitFor } from "@testing-library/react";

import { CopyButton } from "@shared/components/molecules/button/copy";

describe("components/molecules/button/CopyButton", () => {
  beforeEach(() => {
    Object.defineProperty(navigator, "clipboard", {
      value: { writeText: vi.fn().mockResolvedValue(undefined) },
      writable: true,
      configurable: true,
    });
  });

  it("コピーボタンが描画される", () => {
    const { container } = render(<CopyButton text="test code" />);
    const button = container.querySelector("button");
    expect(button).not.toBeNull();
  });

  it("ルートの className が container である", () => {
    const { container } = render(<CopyButton text="test code" />);
    const button = container.querySelector("button");
    expect(button?.className).toContain("container");
  });

  it("クリックするとクリップボードにテキストがコピーされる", async () => {
    const { container } = render(<CopyButton text="copied text" />);
    const button = container.querySelector("button")!;
    await act(async () => {
      fireEvent.click(button);
    });
    expect(navigator.clipboard.writeText).toHaveBeenCalledWith("copied text");
  });

  it("コピー後に copied 状態になる", async () => {
    const { container } = render(<CopyButton text="test" />);
    const button = container.querySelector("button")!;
    await act(async () => {
      fireEvent.click(button);
    });
    await waitFor(() => {
      expect(button.getAttribute("data-copied")).toBe("true");
    });
  });

  it("一定時間後に copied 状態がリセットされる", async () => {
    vi.useFakeTimers();
    const { container } = render(<CopyButton text="test" />);
    const button = container.querySelector("button")!;
    await act(async () => {
      fireEvent.click(button);
    });
    expect(button.getAttribute("data-copied")).toBe("true");
    await act(async () => {
      vi.advanceTimersByTime(2000);
    });
    expect(button.getAttribute("data-copied")).toBe("false");
    vi.useRealTimers();
  });

  it("コピー前はClipboardIconが表示される", () => {
    const { container } = render(<CopyButton text="test" />);
    const button = container.querySelector("button")!;
    expect(button.getAttribute("data-copied")).toBe("false");
    const iconSpan = button.querySelector("span[role='img']");
    expect(iconSpan).not.toBeNull();
  });

  it("コピー後はCheckIconが表示される", async () => {
    const { container } = render(<CopyButton text="test" />);
    const button = container.querySelector("button")!;
    await act(async () => {
      fireEvent.click(button);
    });
    await waitFor(() => {
      expect(button.getAttribute("data-copied")).toBe("true");
    });
    const iconSpan = button.querySelector("span[role='img']");
    expect(iconSpan).not.toBeNull();
  });
});
