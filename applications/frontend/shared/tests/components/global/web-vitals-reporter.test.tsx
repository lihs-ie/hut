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
const setMeasurementMock = vi.fn();

vi.mock("@sentry/nextjs", () => ({
  captureMessage: (message: string, context?: unknown) =>
    captureMessageMock(message, context),
  setMeasurement: (name: string, value: number, unit: string) =>
    setMeasurementMock(name, value, unit),
}));

const buildMetric = (
  name: WebVitalMetric["name"],
  value: number,
  rating: WebVitalMetric["rating"] = "good",
): WebVitalMetric => ({
  id: `metric-${name}`,
  name,
  value,
  rating,
  delta: value,
  entries: [],
  navigationType: "navigate",
});

describe("components/global/web-vitals-reporter", () => {
  beforeEach(() => {
    capturedCallbacks.length = 0;
    captureMessageMock.mockReset();
    setMeasurementMock.mockReset();
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

  it("全メトリクスは setMeasurement で既存トランザクションへ集約される（イベント追加なし）", async () => {
    const module = await import(
      "@shared/components/global/web-vitals-reporter"
    );
    const { WebVitalsReporter } = module;
    const Component = WebVitalsReporter as () => ReactNode;

    render(<Component />);

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
      reportCallback(buildMetric(name, (index + 1) * 100, "good"));
    });

    expect(setMeasurementMock).toHaveBeenCalledTimes(metricNames.length);
    metricNames.forEach((name) => {
      const matched = setMeasurementMock.mock.calls.some(
        ([metricName]) => metricName === name,
      );
      expect(matched).toBe(true);
    });
  });

  it("CLS は単位なし（unitless）、他は millisecond で送信される", async () => {
    const module = await import(
      "@shared/components/global/web-vitals-reporter"
    );
    const { WebVitalsReporter } = module;
    const Component = WebVitalsReporter as () => ReactNode;

    render(<Component />);

    const reportCallback = capturedCallbacks[0]!;

    reportCallback(buildMetric("CLS", 0.08, "good"));
    reportCallback(buildMetric("LCP", 2400, "good"));

    expect(setMeasurementMock).toHaveBeenCalledWith("CLS", 0.08, "");
    expect(setMeasurementMock).toHaveBeenCalledWith(
      "LCP",
      2400,
      "millisecond",
    );
  });

  it("rating が good の場合は captureMessage を呼ばない（event quota 節約）", async () => {
    const module = await import(
      "@shared/components/global/web-vitals-reporter"
    );
    const { WebVitalsReporter } = module;
    const Component = WebVitalsReporter as () => ReactNode;

    render(<Component />);

    const reportCallback = capturedCallbacks[0]!;
    reportCallback(buildMetric("LCP", 2000, "good"));

    expect(captureMessageMock).not.toHaveBeenCalled();
  });

  it("rating が needs-improvement の場合も captureMessage を呼ばない", async () => {
    const module = await import(
      "@shared/components/global/web-vitals-reporter"
    );
    const { WebVitalsReporter } = module;
    const Component = WebVitalsReporter as () => ReactNode;

    render(<Component />);

    const reportCallback = capturedCallbacks[0]!;
    reportCallback(buildMetric("LCP", 3500, "needs-improvement"));

    expect(captureMessageMock).not.toHaveBeenCalled();
  });

  it("rating が poor の場合のみ captureMessage でイベント送信する", async () => {
    const module = await import(
      "@shared/components/global/web-vitals-reporter"
    );
    const { WebVitalsReporter } = module;
    const Component = WebVitalsReporter as () => ReactNode;

    render(<Component />);

    const reportCallback = capturedCallbacks[0]!;
    reportCallback(buildMetric("LCP", 5000, "poor"));

    expect(captureMessageMock).toHaveBeenCalledTimes(1);
    const [message, context] = captureMessageMock.mock.calls[0]!;
    expect(message).toContain("LCP");
    const payload = context as {
      level?: string;
      tags?: Record<string, string | undefined>;
      extra?: Record<string, unknown>;
    };
    expect(payload.level).toBe("error");
    expect(payload.tags?.["web_vital"]).toBe("LCP");
    expect(payload.tags?.["rating"]).toBe("poor");
    expect(payload.extra?.value).toBe(5000);
  });
});
