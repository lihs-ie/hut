"use client";

import { useReportWebVitals } from "next/web-vitals";
import { captureMessage, setMeasurement } from "@sentry/nextjs";

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

const getMeasurementUnit = (metricName: string): "" | "millisecond" =>
  metricName === "CLS" ? "" : "millisecond";

export const WebVitalsReporter = (_props: Props) => {
  void _props;

  useReportWebVitals((metric: WebVitalMetric) => {
    setMeasurement(metric.name, metric.value, getMeasurementUnit(metric.name));

    if (metric.rating !== "poor") {
      return;
    }

    captureMessage(`Web Vital: ${metric.name}`, {
      level: "error",
      tags: {
        web_vital: metric.name,
        rating: metric.rating,
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
