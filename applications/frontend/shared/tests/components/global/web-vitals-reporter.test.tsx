/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, cleanup } from "@testing-library/react";
import type { ReactNode } from "react";

type WebVitalMetric = {
  id: string;
  name: "CLS" | "LCP" | "FCP" | "INP" | "TTFB" | "FID";
  value: number;
  rating: "good" | "needs-improvement" | "poor";
  delta: number;
  entries: PerformanceEntry[];
  navigationType:
    | "navigate"
    | "reload"
    | "back-forward"
    | "back-forward-cache"
    | "prerender"
    | "restore";
};

type ReportCallback = (metric: WebVitalMetric) => void;

const capturedCallbacks: ReportCallback[] = [];

vi.mock("next/web-vitals", () => ({
  useReportWebVitals: (callback: ReportCallback) => {
    capturedCallbacks.push(callback);
  },
}));

const captureMessageMock = vi.fn();

vi.mock("@sentry/nextjs", () => ({
  captureMessage: (message: string, context?: unknown) =>
    captureMessageMock(message, context),
}));

const buildMetric = (
  name: WebVitalMetric["name"],
  value: number,
): WebVitalMetric => ({
  id: `metric-${name}`,
  name,
  value,
  rating: "good",
  delta: value,
  entries: [],
  navigationType: "navigate",
});

describe("components/global/web-vitals-reporter", () => {
  beforeEach(() => {
    capturedCallbacks.length = 0;
    captureMessageMock.mockReset();
  });

  afterEach(() => {
    cleanup();
  });

  it("コンポーネントは null を返す（レンダリング要素を持たない）", async () => {
    const module = await import(
      "@shared/components/global/web-vitals-reporter"
    );
    const { WebVitalsReporter } = module;
    const Component = WebVitalsReporter as () => ReactNode;

    const { container } = render(<Component />);

    expect(container.firstChild).toBeNull();
  });

  it("CLS / LCP / FCP / INP / TTFB / FID のメトリクスを Sentry に送信する", async () => {
    const module = await import(
      "@shared/components/global/web-vitals-reporter"
    );
    const { WebVitalsReporter } = module;
    const Component = WebVitalsReporter as () => ReactNode;

    render(<Component />);

    expect(capturedCallbacks.length).toBeGreaterThan(0);
    const reportCallback = capturedCallbacks[0]!;

    const metricNames: WebVitalMetric["name"][] = [
      "CLS",
      "LCP",
      "FCP",
      "INP",
      "TTFB",
      "FID",
    ];

    metricNames.forEach((name, index) => {
      reportCallback(buildMetric(name, (index + 1) * 100));
    });

    expect(captureMessageMock).toHaveBeenCalledTimes(metricNames.length);
    metricNames.forEach((name) => {
      const matched = captureMessageMock.mock.calls.some(([message]) => {
        return typeof message === "string" && message.includes(name);
      });
      expect(matched).toBe(true);
    });
  });

  it("メトリクスの数値とレーティングは context の tags または extra に含まれる", async () => {
    const module = await import(
      "@shared/components/global/web-vitals-reporter"
    );
    const { WebVitalsReporter } = module;
    const Component = WebVitalsReporter as () => ReactNode;

    render(<Component />);

    const reportCallback = capturedCallbacks[0]!;
    reportCallback(buildMetric("LCP", 2400));

    expect(captureMessageMock).toHaveBeenCalledTimes(1);
    const context = captureMessageMock.mock.calls[0]?.[1];
    expect(context).toBeDefined();
    const payload = context as {
      level?: string;
      tags?: Record<string, string | number | undefined>;
      extra?: Record<string, unknown>;
    };
    expect(payload.tags?.["web_vital"]).toBe("LCP");
    expect(payload.tags?.["rating"]).toBe("good");
    expect(payload.extra?.value).toBe(2400);
  });
});
