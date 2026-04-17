import type { NextConfig } from "next";
import type { RemotePattern } from "next/dist/shared/lib/image-config";
import { withSentryConfig, type SentryBuildOptions } from "@sentry/nextjs";
import { parseImageRemotePatterns } from "./shared/src/config/image-remote-pattern";

type Options = {
  readonly useFirebaseEmulator?: boolean;
  readonly includeCSP?: boolean;
  readonly contentSecurityPolicy?: string;
};

const EMULATOR_PATTERNS: ReadonlyArray<RemotePattern> = [
  { protocol: "http", hostname: "localhost" },
];

/**
 * アプリ共通で利用する既定の Content Security Policy を生成する。
 */
const EMULATOR_ORIGIN = "http://localhost:9099";

export const SENTRY_CONNECT_SOURCES =
  "https://*.sentry.io https://*.ingest.sentry.io";

const createDefaultContentSecurityPolicy = (
  isProduction: boolean,
  useEmulator: boolean,
): string =>
  [
    "default-src 'self'",
    isProduction
      ? "script-src 'self' 'unsafe-inline' https://apis.google.com https://*.firebaseio.com"
      : "script-src 'self' 'unsafe-inline' 'unsafe-eval' https://apis.google.com https://*.firebaseio.com",
    "style-src 'self' 'unsafe-inline' https://fonts.googleapis.com",
    "font-src 'self' https://fonts.gstatic.com",
    "img-src 'self' data: blob: https:",
    `connect-src 'self' https://*.googleapis.com https://*.firebaseio.com wss://*.firebaseio.com https://identitytoolkit.googleapis.com ${SENTRY_CONNECT_SOURCES}${useEmulator ? ` ${EMULATOR_ORIGIN}` : ""}`,
    `frame-src 'self' https://accounts.google.com https://*.firebaseapp.com${useEmulator ? ` ${EMULATOR_ORIGIN}` : ""}`,
    "object-src 'none'",
    "base-uri 'self'",
    "form-action 'self'",
  ].join("; ");

const createSentryBuildOptions = (): SentryBuildOptions => ({
  silent: !process.env.CI,
  org: process.env.SENTRY_ORG,
  project: process.env.SENTRY_PROJECT,
  authToken: process.env.SENTRY_AUTH_TOKEN,
  widenClientFileUpload: true,
  sourcemaps: {
    deleteSourcemapsAfterUpload: true,
  },
  release: {
    name: process.env.NEXT_PUBLIC_GIT_SHA,
  },
});

/**
 * Sentry 設定で Next.js 設定をラップする。
 */
export const wrapWithSentryConfig = <C extends NextConfig>(nextConfig: C): C =>
  withSentryConfig(nextConfig, createSentryBuildOptions());

/**
 * frontend アプリ共通の Next.js 設定を生成する。
 */
export const createBaseNextConfig = (options?: Options): NextConfig => {
  const isProduction = process.env.NODE_ENV === "production";
  const useEmulator = options?.useFirebaseEmulator ?? false;
  const includeCSP = options?.includeCSP ?? true;
  const contentSecurityPolicy =
    options?.contentSecurityPolicy ??
    createDefaultContentSecurityPolicy(isProduction, useEmulator);

  const remotePatterns: Array<RemotePattern> = [
    ...parseImageRemotePatterns(process.env.IMAGE_REMOTE_PATTERNS),
    ...(useEmulator ? EMULATOR_PATTERNS : []),
  ];

  const allowedOrigins = process.env.SERVER_ACTIONS_ALLOWED_ORIGINS
    ? process.env.SERVER_ACTIONS_ALLOWED_ORIGINS.split(",")
    : [];

  return {
    output: "standalone",
    poweredByHeader: false,
    headers: async () => [
      {
        source: "/(.*)",
        headers: [
          { key: "X-Content-Type-Options", value: "nosniff" },
          { key: "X-Frame-Options", value: "DENY" },
          {
            key: "Referrer-Policy",
            value: "strict-origin-when-cross-origin",
          },
          {
            key: "Permissions-Policy",
            value: "camera=(), microphone=(), geolocation=()",
          },
          {
            key: "Strict-Transport-Security",
            value: "max-age=31536000; includeSubDomains; preload",
          },
          {
            key: "Cross-Origin-Opener-Policy",
            value: "same-origin-allow-popups",
          },
          {
            key: "Cross-Origin-Resource-Policy",
            value: "same-origin",
          },
          {
            key: "X-Permitted-Cross-Domain-Policies",
            value: "none",
          },
          ...(includeCSP
            ? [
                {
                  key: "Content-Security-Policy",
                  value: contentSecurityPolicy,
                },
              ]
            : []),
        ],
      },
    ],
    turbopack: {
      root: "../..",
    },
    reactCompiler: true,
    pageExtensions: ["tsx", "ts", "jsx", "js", "md", "mdx"],
    transpilePackages: ["@hut/shared"],
    images: {
      remotePatterns,
      ...(useEmulator && { dangerouslyAllowLocalIP: true }),
    },
    experimental: {
      serverActions: { allowedOrigins },
    },
  };
};
