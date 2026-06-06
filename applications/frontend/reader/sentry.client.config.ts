import * as Sentry from "@sentry/nextjs";
import { filterSensitiveEvent } from "@shared/observability/sentry-filter";

Sentry.init({
  dsn: process.env.NEXT_PUBLIC_SENTRY_DSN_READER,
  environment:
    process.env.NEXT_PUBLIC_SENTRY_ENVIRONMENT ?? process.env.NODE_ENV,
  release: process.env.NEXT_PUBLIC_GIT_SHA,
  tracesSampleRate: 0.1,
  sendDefaultPii: false,
  beforeSend: filterSensitiveEvent,
});
