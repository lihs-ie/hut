import * as Sentry from "@sentry/nextjs";
import { createSentryConfiguration } from "@shared/aspects/sentry";

Sentry.init({
  ...createSentryConfiguration(
    process.env.NEXT_PUBLIC_SENTRY_DSN,
    process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR,
  ),
});
