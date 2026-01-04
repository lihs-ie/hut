import type { NextConfig } from "next";
import MDX from "@next/mdx";

const nextConfig: NextConfig = {
  reactCompiler: true,
  pageExtensions: ["tsx", "ts", "jsx", "js", "md", "mdx"],
  transpilePackages: ["@hut/shared"],
};

const withMDX = MDX({
  extension: /\.mdx?$/,
});

export default withMDX(nextConfig);
