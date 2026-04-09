import type { MetadataRoute } from "next";

export default function robots(): MetadataRoute.Robots {
  if (process.env.DISALLOW_ROBOTS === "true") {
    return {
      rules: { userAgent: "*", disallow: "/" },
    };
  }

  const siteUrl = process.env.NEXT_PUBLIC_SITE_URL ?? "https://hut.lihs.dev";

  return {
    rules: { userAgent: "*", allow: "/" },
    sitemap: `${siteUrl}/sitemap.xml`,
  };
}
