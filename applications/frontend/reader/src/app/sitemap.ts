import type { MetadataRoute } from "next";
import { connection } from "next/server";
import { searchAllSlugs as searchArticleSlugs } from "@/actions/article";

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  await connection();
  const siteUrl = process.env.NEXT_PUBLIC_SITE_URL ?? "https://hut.lihs.dev";

  const articleSlugs = await searchArticleSlugs();

  const toUrl = (path: string) => new URL(path, siteUrl).toString();

  const staticPages: MetadataRoute.Sitemap = [
    { url: toUrl("/"), changeFrequency: "weekly", priority: 1.0 },
    { url: toUrl("/articles"), changeFrequency: "daily", priority: 0.9 },
    { url: toUrl("/about"), changeFrequency: "monthly", priority: 0.6 },
    { url: toUrl("/privacy"), changeFrequency: "yearly", priority: 0.3 },
  ];

  const articleEntries: MetadataRoute.Sitemap = articleSlugs.map((slug) => ({
    url: toUrl(`/articles/${slug}`),
    changeFrequency: "monthly",
    priority: 0.7,
  }));

  return [...staticPages, ...articleEntries];
}
