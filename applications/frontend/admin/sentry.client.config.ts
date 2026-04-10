import * as Sentry from "@sentry/nextjs";
import { createSentryConfiguration } from "@shared/aspects/sentry";

Sentry.init({
  ...createSentryConfiguration(
    process.env.NEXT_PUBLIC_SENTRY_DSN,
    process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR,
  ),
  replaysSessionSampleRate: 0.1,
  replaysOnErrorSampleRate: 1.0,
  integrations: [Sentry.replayIntegration()],
});
