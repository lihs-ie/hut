import type { NextConfig } from "next";
import type { RemotePattern } from "next/dist/shared/lib/image-config";

type Options = {
  readonly additionalRemotePatterns?: ReadonlyArray<RemotePattern>;
  readonly dangerouslyAllowLocalIP?: boolean;
};

export const createBaseNextConfig = (options?: Options): NextConfig => {
  const remotePatterns: Array<RemotePattern> = [
    {
      protocol: "https",
      hostname: "**",
    },
    ...(options?.additionalRemotePatterns ?? []),
  ];

  const allowedOrigins = process.env.SERVER_ACTIONS_ALLOWED_ORIGINS
    ? process.env.SERVER_ACTIONS_ALLOWED_ORIGINS.split(",")
    : undefined;

  return {
    output: "standalone",
    turbopack: {
      root: "../..",
    },
    reactCompiler: true,
    pageExtensions: ["tsx", "ts", "jsx", "js", "md", "mdx"],
    transpilePackages: ["@hut/shared"],
    images: {
      remotePatterns,
      ...(options?.dangerouslyAllowLocalIP !== undefined && {
        dangerouslyAllowLocalIP: options.dangerouslyAllowLocalIP,
      }),
    },
    ...(allowedOrigins && {
      experimental: {
        serverActions: { allowedOrigins },
      },
    }),
  };
};
