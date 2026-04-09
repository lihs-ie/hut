import type { MetadataRoute } from "next";
import { searchAllSlugs as searchArticleSlugs } from "@/actions/article";
import { searchAllSlugs as searchMemoSlugs } from "@/actions/memo";
import { searchAllSlugs as searchSeriesSlugs } from "@/actions/series";

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const siteUrl = process.env.NEXT_PUBLIC_SITE_URL ?? "https://hut.lihs.dev";

  const [articleSlugs, memoSlugs, seriesSlugs] = await Promise.all([
    searchArticleSlugs(),
    searchMemoSlugs(),
    searchSeriesSlugs(),
  ]);

  const staticPages: MetadataRoute.Sitemap = [
    { url: `${siteUrl}/`, changeFrequency: "weekly", priority: 1.0 },
    { url: `${siteUrl}/articles`, changeFrequency: "daily", priority: 0.9 },
    { url: `${siteUrl}/memos`, changeFrequency: "daily", priority: 0.9 },
    { url: `${siteUrl}/series`, changeFrequency: "weekly", priority: 0.8 },
    { url: `${siteUrl}/about`, changeFrequency: "monthly", priority: 0.6 },
    { url: `${siteUrl}/privacy`, changeFrequency: "yearly", priority: 0.3 },
  ];

  const articleEntries: MetadataRoute.Sitemap = articleSlugs.map((slug) => ({
    url: `${siteUrl}/articles/${slug}`,
    changeFrequency: "monthly",
    priority: 0.7,
  }));

  const memoEntries: MetadataRoute.Sitemap = memoSlugs.map((slug) => ({
    url: `${siteUrl}/memos/${slug}`,
    changeFrequency: "monthly",
    priority: 0.7,
  }));

  const seriesEntries: MetadataRoute.Sitemap = seriesSlugs.map((slug) => ({
    url: `${siteUrl}/series/${slug}`,
    changeFrequency: "monthly",
    priority: 0.8,
  }));

  return [...staticPages, ...articleEntries, ...memoEntries, ...seriesEntries];
}
