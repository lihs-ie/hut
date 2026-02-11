import type { NextConfig } from "next";
import type { RemotePattern } from "next/dist/shared/lib/image-config";

type Options = {
  readonly additionalRemotePatterns?: ReadonlyArray<RemotePattern>;
};

export const createBaseNextConfig = (options?: Options): NextConfig => {
  const remotePatterns: Array<RemotePattern> = [
    {
      protocol: "https",
      hostname: "**",
    },
    ...(options?.additionalRemotePatterns ?? []),
  ];

  return {
    turbopack: {
      root: "../..",
    },
    reactCompiler: true,
    pageExtensions: ["tsx", "ts", "jsx", "js", "md", "mdx"],
    transpilePackages: ["@hut/shared"],
    images: {
      remotePatterns,
    },
  };
};
