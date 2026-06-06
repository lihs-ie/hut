import MDX from "@next/mdx";
import {
  createBaseNextConfig,
  wrapWithSentryConfig,
} from "../next.config.shared";

const withMDX = MDX({ extension: /\.mdx?$/ });

export default wrapWithSentryConfig(
  withMDX(
    createBaseNextConfig({
      useFirebaseEmulator:
        process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true",
    }),
  ),
);
