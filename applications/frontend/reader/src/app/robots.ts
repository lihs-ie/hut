import type { MetadataRoute } from "next";

export default function robots(): MetadataRoute.Robots {
  if (process.env.DISALLOW_ROBOTS === "true") {
    return {
      rules: { userAgent: "*", disallow: "/" },
    };
  }

  return {
    rules: { userAgent: "*", allow: "/" },
  };
}
