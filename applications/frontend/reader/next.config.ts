import MDX from "@next/mdx";
import { createBaseNextConfig } from "../next.config.shared";

const withMDX = MDX({ extension: /\.mdx?$/ });

export default withMDX(
  createBaseNextConfig({
    additionalRemotePatterns: [
      {
        protocol: "http",
        hostname: "localhost",
      },
    ],
  }),
);
