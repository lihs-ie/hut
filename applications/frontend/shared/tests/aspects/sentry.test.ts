import { describe, it, expect } from "vitest";
import { createSentryConfiguration } from "@shared/aspects/sentry";

describe("createSentryConfiguration", () => {
  it("DSNが設定されている場合、enabledがtrueになる", () => {
    const configuration = createSentryConfiguration(
      "https://xxx@sentry.io/123",
      undefined,
    );
    expect(configuration.enabled).toBe(true);
    expect(configuration.dsn).toBe("https://xxx@sentry.io/123");
  });

  it("DSNが未設定の場合、enabledがfalseになる", () => {
    const configuration = createSentryConfiguration(undefined, undefined);
    expect(configuration.enabled).toBe(false);
  });

  it("DSNが空文字の場合、enabledがfalseになる", () => {
    const configuration = createSentryConfiguration("", undefined);
    expect(configuration.enabled).toBe(false);
  });

  it("DSNが空白のみの場合、enabledがfalseになる", () => {
    const configuration = createSentryConfiguration("  ", undefined);
    expect(configuration.enabled).toBe(false);
    expect(configuration.dsn).toBeUndefined();
  });

  it("エミュレータ使用時はenabledがfalseになる", () => {
    const configuration = createSentryConfiguration(
      "https://xxx@sentry.io/123",
      "true",
    );
    expect(configuration.enabled).toBe(false);
  });

  it("エミュレータ未使用時かつDSN設定済みならenabledがtrueになる", () => {
    const configuration = createSentryConfiguration(
      "https://xxx@sentry.io/123",
      "false",
    );
    expect(configuration.enabled).toBe(true);
  });

  it("tracesSampleRateがデフォルト値0.1である", () => {
    const configuration = createSentryConfiguration(
      "https://xxx@sentry.io/123",
      undefined,
    );
    expect(configuration.tracesSampleRate).toBe(0.1);
  });
});
