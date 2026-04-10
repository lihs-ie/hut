import MDX from "@next/mdx";
import { withSentryConfig } from "@sentry/nextjs";
import { createBaseNextConfig } from "../next.config.shared";

const withMDX = MDX({ extension: /\.mdx?$/ });

export default withSentryConfig(
  withMDX(
    createBaseNextConfig({
      useFirebaseEmulator:
        process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true",
    }),
  ),
  {
    org: process.env.SENTRY_ORG,
    project: process.env.SENTRY_PROJECT,
    silent: !process.env.CI,
    widenClientFileUpload: true,
    disableLogger: true,
    authToken: process.env.SENTRY_AUTH_TOKEN,
    sourcemaps: { deleteSourcemapsAfterUpload: true },
    tunnelRoute: "/monitoring",
  },
);
