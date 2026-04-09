import { search as searchArticles } from "@shared/actions/article";
import { search as searchMemos } from "@shared/actions/memo";

const SITE_TITLE = "hut";
const SITE_DESCRIPTION = "個人的な技術学習記事やメモを公開するためのプラットフォームです。";

type FeedItem = {
  title: string;
  link: string;
  description: string;
  pubDate: string;
  guid: string;
};

function escapeXml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&apos;");
}

function buildRssXml(siteUrl: string, items: FeedItem[]): string {
  const escapedSiteUrl = escapeXml(siteUrl);

  const itemsXml = items
    .map(
      (item) => `
    <item>
      <title><![CDATA[${item.title}]]></title>
      <link>${escapeXml(item.link)}</link>
      <description><![CDATA[${item.description}]]></description>
      <pubDate>${escapeXml(item.pubDate)}</pubDate>
      <guid>${escapeXml(item.guid)}</guid>
    </item>`,
    )
    .join("");

  return `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title><![CDATA[${SITE_TITLE}]]></title>
    <link>${escapedSiteUrl}</link>
    <description><![CDATA[${SITE_DESCRIPTION}]]></description>
    <language>ja</language>
    <atom:link href="${escapedSiteUrl}/feed.xml" rel="self" type="application/rss+xml" />
    ${itemsXml}
  </channel>
</rss>`;
}

export async function GET(): Promise<Response> {
  const siteUrl = process.env.NEXT_PUBLIC_SITE_URL ?? "https://hut.lihs.dev";

  const [articles, memos] = await Promise.all([
    searchArticles({}),
    searchMemos({ tags: null, freeWord: null, status: null }),
  ]);

  const articleItems: FeedItem[] = articles.map((article) => ({
    title: article.title,
    link: `${siteUrl}/articles/${article.slug}`,
    description: article.excerpt,
    pubDate: article.timeline.updatedAt.toUTCString(),
    guid: `${siteUrl}/articles/${article.slug}`,
  }));

  const memoItems: FeedItem[] = memos.map((memo) => ({
    title: memo.title,
    link: `${siteUrl}/memos/${memo.slug}`,
    description: memo.entries.length > 0 ? memo.entries[0].text : "",
    pubDate: memo.timeline.updatedAt.toUTCString(),
    guid: `${siteUrl}/memos/${memo.slug}`,
  }));

  const allItems = [...articleItems, ...memoItems].sort(
    (a, b) => new Date(b.pubDate).getTime() - new Date(a.pubDate).getTime(),
  );

  const xml = buildRssXml(siteUrl, allItems);

  return new Response(xml, {
    headers: {
      "Content-Type": "application/xml; charset=utf-8",
      "Cache-Control": "public, max-age=3600, s-maxage=3600",
    },
  });
}
