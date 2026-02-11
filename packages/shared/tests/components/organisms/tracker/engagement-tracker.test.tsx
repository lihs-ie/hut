/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, cleanup, act } from "@testing-library/react";
import { EngagementTracker } from "@shared/components/organisms/tracker";
import { ContentType } from "@shared/domains/search-token/reference";

const mockObserve = vi.fn();
const mockDisconnect = vi.fn();
const mockUnobserve = vi.fn();

let intersectionCallback: IntersectionObserverCallback;

class MockIntersectionObserver implements IntersectionObserver {
  readonly root: Element | Document | null = null;
  readonly rootMargin: string = "";
  readonly thresholds: ReadonlyArray<number> = [0.5];

  constructor(
    callback: IntersectionObserverCallback,
    _options?: IntersectionObserverInit,
  ) {
    intersectionCallback = callback;
    MockIntersectionObserver.constructorSpy(callback, _options);
  }

  static constructorSpy = vi.fn();

  observe(target: Element): void {
    mockObserve(target);
  }

  disconnect(): void {
    mockDisconnect();
  }

  unobserve(target: Element): void {
    mockUnobserve(target);
  }

  takeRecords(): IntersectionObserverEntry[] {
    return [];
  }
}

let lastBeaconPayload: string | null = null;
const mockSendBeacon = vi.fn((_url: string, _data: Blob) => {
  return true;
});

const OriginalBlob = globalThis.Blob;

class CapturingBlob extends OriginalBlob {
  readonly capturedText: string;

  constructor(parts?: BlobPart[], options?: BlobPropertyBag) {
    super(parts, options);
    this.capturedText = parts
      ? parts.map((part) => (typeof part === "string" ? part : "")).join("")
      : "";
    lastBeaconPayload = this.capturedText;
  }
}

describe("EngagementTracker", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    vi.stubGlobal("IntersectionObserver", MockIntersectionObserver);
    vi.stubGlobal("Blob", CapturingBlob);
    lastBeaconPayload = null;
    Object.defineProperty(navigator, "sendBeacon", {
      value: mockSendBeacon,
      writable: true,
      configurable: true,
    });

    const contentElement = document.createElement("div");
    contentElement.id = "article-content";
    contentElement.style.position = "relative";
    Object.defineProperty(contentElement, "scrollHeight", {
      value: 2000,
      configurable: true,
    });
    document.body.appendChild(contentElement);
  });

  afterEach(() => {
    cleanup();
    vi.useRealTimers();
    vi.clearAllMocks();
    document.body.innerHTML = "";
  });

  it("UIを描画しないこと", () => {
    const { container } = render(
      <EngagementTracker
        contentType={ContentType.ARTICLE}
        contentIdentifier="01JFABCDEF1234567890ABCDEF"
        contentElementId="article-content"
      />,
    );

    expect(container.innerHTML).toBe("");
  });

  it("IntersectionObserverを初期化してスクロール深度マーカーを監視すること", () => {
    render(
      <EngagementTracker
        contentType={ContentType.ARTICLE}
        contentIdentifier="01JFABCDEF1234567890ABCDEF"
        contentElementId="article-content"
      />,
    );

    expect(MockIntersectionObserver.constructorSpy).toHaveBeenCalledWith(
      expect.any(Function),
      { threshold: 0.5 },
    );

    expect(mockObserve).toHaveBeenCalledTimes(4);
  });

  it("visibilitychangeイベントでアクティブ時間を計測すること", () => {
    render(
      <EngagementTracker
        contentType={ContentType.ARTICLE}
        contentIdentifier="01JFABCDEF1234567890ABCDEF"
        contentElementId="article-content"
      />,
    );

    vi.advanceTimersByTime(5000);
    Object.defineProperty(document, "hidden", {
      value: true,
      configurable: true,
    });
    document.dispatchEvent(new Event("visibilitychange"));

    Object.defineProperty(document, "hidden", {
      value: false,
      configurable: true,
    });
    document.dispatchEvent(new Event("visibilitychange"));

    vi.advanceTimersByTime(3000);
    window.dispatchEvent(new Event("beforeunload"));

    expect(mockSendBeacon).toHaveBeenCalledTimes(1);
    const [url, blob] = mockSendBeacon.mock.calls[0];
    expect(url).toBe("/api/engagement");
    expect(blob).toBeInstanceOf(Blob);
  });

  it("スクロール深度をIntersectionObserverコールバックで更新すること", () => {
    render(
      <EngagementTracker
        contentType={ContentType.ARTICLE}
        contentIdentifier="01JFABCDEF1234567890ABCDEF"
        contentElementId="article-content"
      />,
    );

    const savedCallback = intersectionCallback;

    const marker25 = document.createElement("div");
    marker25.setAttribute("data-depth", "25");

    act(() => {
      savedCallback(
        [
          {
            isIntersecting: true,
            target: marker25,
            boundingClientRect: {} as DOMRectReadOnly,
            intersectionRatio: 0.5,
            intersectionRect: {} as DOMRectReadOnly,
            rootBounds: null,
            time: 0,
          },
        ],
        {} as IntersectionObserver,
      );
    });

    const marker50 = document.createElement("div");
    marker50.setAttribute("data-depth", "50");

    act(() => {
      savedCallback(
        [
          {
            isIntersecting: true,
            target: marker50,
            boundingClientRect: {} as DOMRectReadOnly,
            intersectionRatio: 0.5,
            intersectionRect: {} as DOMRectReadOnly,
            rootBounds: null,
            time: 0,
          },
        ],
        {} as IntersectionObserver,
      );
    });

    window.dispatchEvent(new Event("beforeunload"));

    expect(mockSendBeacon).toHaveBeenCalledTimes(1);
    expect(lastBeaconPayload).not.toBeNull();
    const data = JSON.parse(lastBeaconPayload!);

    expect(data.maxScrollDepth).toBe(50);
    expect(data.contentType).toBe("article");
    expect(data.contentIdentifier).toBe("01JFABCDEF1234567890ABCDEF");
  });

  it("二重送信を防止すること", () => {
    render(
      <EngagementTracker
        contentType={ContentType.ARTICLE}
        contentIdentifier="01JFABCDEF1234567890ABCDEF"
        contentElementId="article-content"
      />,
    );

    window.dispatchEvent(new Event("beforeunload"));
    window.dispatchEvent(new Event("beforeunload"));

    expect(mockSendBeacon).toHaveBeenCalledTimes(1);
  });

  it("コンテンツ要素が存在しない場合はIntersectionObserverを初期化しないこと", () => {
    render(
      <EngagementTracker
        contentType={ContentType.ARTICLE}
        contentIdentifier="01JFABCDEF1234567890ABCDEF"
        contentElementId="nonexistent-element"
      />,
    );

    expect(MockIntersectionObserver.constructorSpy).not.toHaveBeenCalled();
    expect(mockObserve).not.toHaveBeenCalled();
  });

  it("アンマウント時にクリーンアップしてビーコンを送信すること", () => {
    const { unmount } = render(
      <EngagementTracker
        contentType={ContentType.ARTICLE}
        contentIdentifier="01JFABCDEF1234567890ABCDEF"
        contentElementId="article-content"
      />,
    );

    unmount();

    expect(mockDisconnect).toHaveBeenCalled();
    expect(mockSendBeacon).toHaveBeenCalledTimes(1);
  });

  it("コンテンツ要素に4つのマーカー要素を追加すること", () => {
    render(
      <EngagementTracker
        contentType={ContentType.ARTICLE}
        contentIdentifier="01JFABCDEF1234567890ABCDEF"
        contentElementId="article-content"
      />,
    );

    const contentElement = document.getElementById("article-content");
    expect(contentElement).not.toBeNull();

    const markers = contentElement!.querySelectorAll("[data-depth]");
    expect(markers.length).toBe(4);

    const depths = Array.from(markers).map((marker) =>
      marker.getAttribute("data-depth"),
    );
    expect(depths).toEqual(["25", "50", "75", "100"]);
  });
});
