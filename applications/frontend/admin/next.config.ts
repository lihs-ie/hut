import MDX from "@next/mdx";
import { createBaseNextConfig } from "../next.config.shared";

const withMDX = MDX({ extension: /\.mdx?$/ });

export default withMDX(
  createBaseNextConfig({
    additionalRemotePatterns: [
      {
        protocol: "http",
        hostname: "**",
      },
    ],
    dangerouslyAllowLocalIP:
      process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true",
  }),
);
