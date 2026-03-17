import type { NextConfig } from "next";
import type { RemotePattern } from "next/dist/shared/lib/image-config";
import { parseImageRemotePatterns } from "./shared/src/config/image-remote-pattern";

type Options = {
  readonly useFirebaseEmulator?: boolean;
};

const EMULATOR_PATTERNS: ReadonlyArray<RemotePattern> = [
  { protocol: "http", hostname: "localhost" },
];

export const createBaseNextConfig = (options?: Options): NextConfig => {
  const useEmulator = options?.useFirebaseEmulator ?? false;

  const remotePatterns: Array<RemotePattern> = [
    ...parseImageRemotePatterns(process.env.IMAGE_REMOTE_PATTERNS),
    ...(useEmulator ? EMULATOR_PATTERNS : []),
  ];

  const allowedOrigins = process.env.SERVER_ACTIONS_ALLOWED_ORIGINS
    ? process.env.SERVER_ACTIONS_ALLOWED_ORIGINS.split(",")
    : undefined;

  return {
    output: "standalone",
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
            value: "max-age=31536000; includeSubDomains",
          },
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
    ...(allowedOrigins && {
      experimental: {
        serverActions: { allowedOrigins },
      },
    }),
  };
};
