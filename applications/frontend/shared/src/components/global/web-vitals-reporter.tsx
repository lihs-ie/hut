"use client";

import { useReportWebVitals } from "next/web-vitals";
import { captureMessage } from "@sentry/nextjs";

type Props = Record<string, never>;

type WebVitalRating = "good" | "needs-improvement" | "poor";

type WebVitalMetric = {
  id: string;
  name: string;
  value: number;
  rating?: WebVitalRating;
  delta: number;
  navigationType?: string;
};

const toSeverityLevel = (
  rating: WebVitalRating | undefined,
): "info" | "warning" | "error" => {
  if (rating === "poor") {
    return "error";
  }
  if (rating === "needs-improvement") {
    return "warning";
  }
  return "info";
};

export const WebVitalsReporter = (_props: Props) => {
  void _props;

  useReportWebVitals((metric: WebVitalMetric) => {
    captureMessage(`Web Vital: ${metric.name}`, {
      level: toSeverityLevel(metric.rating),
      tags: {
        web_vital: metric.name,
        rating: metric.rating ?? "unknown",
      },
      extra: {
        value: metric.value,
        delta: metric.delta,
        id: metric.id,
        navigationType: metric.navigationType,
      },
    });
  });

  return null;
};
