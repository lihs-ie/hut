import {
  searchArticles,
  searchMemos,
  searchSeries,
} from "@/actions/feed/article-search";
import { findPublishedChaptersByIdentifiers } from "@/actions/chapter";

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

const FEED_MAX_ITEMS = 50;

function buildRssXml(siteUrl: string, items: FeedItem[]): string {
  const escapedSiteUrl = escapeXml(siteUrl);
  const feedUrl = escapeXml(new URL("/feed.xml", siteUrl).toString());

  const itemsXml = items
    .slice(0, FEED_MAX_ITEMS)
    .map(
      (item) => `
    <item>
      <title>${escapeXml(item.title)}</title>
      <link>${escapeXml(item.link)}</link>
      <description>${escapeXml(item.description)}</description>
      <pubDate>${escapeXml(item.pubDate)}</pubDate>
      <guid>${escapeXml(item.guid)}</guid>
    </item>`,
    )
    .join("");

  return `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>${escapeXml(SITE_TITLE)}</title>
    <link>${escapedSiteUrl}</link>
    <description>${escapeXml(SITE_DESCRIPTION)}</description>
    <language>ja</language>
    <atom:link href="${feedUrl}" rel="self" type="application/rss+xml" />
    ${itemsXml}
  </channel>
</rss>`;
}

export async function GET(): Promise<Response> {
  const siteUrl = process.env.NEXT_PUBLIC_SITE_URL ?? "https://hut.lihs.dev";

  const [articles, memos, seriesList] = await Promise.all([
    searchArticles({}),
    searchMemos({ tags: null, freeWord: null, status: null }),
    searchSeries(),
  ]);

  const allChapterIdentifiers = [
    ...new Set(seriesList.flatMap((series) => series.chapters)),
  ];
  const allChapters =
    allChapterIdentifiers.length > 0
      ? await findPublishedChaptersByIdentifiers(allChapterIdentifiers)
      : [];
  const chapterByIdentifier = new Map(
    allChapters.map((chapter) => [chapter.identifier, chapter] as const),
  );
  const chaptersBySeries = seriesList.map((series) => ({
    series,
    chapters: series.chapters
      .map((identifier) => chapterByIdentifier.get(identifier))
      .filter(
        (chapter): chapter is (typeof allChapters)[number] =>
          chapter !== undefined,
      ),
  }));

  const toUrl = (path: string) => new URL(path, siteUrl).toString();

  const articleItems: FeedItem[] = articles.map((article) => ({
    title: article.title,
    link: toUrl(`/articles/${article.slug}`),
    description: article.excerpt,
    pubDate: article.timeline.updatedAt.toUTCString(),
    guid: toUrl(`/articles/${article.slug}`),
  }));

  const memoItems: FeedItem[] = memos.map((memo) => ({
    title: memo.title,
    link: toUrl(`/memos/${memo.slug}`),
    description: memo.entries.length > 0 ? memo.entries[0].text : "",
    pubDate: memo.timeline.updatedAt.toUTCString(),
    guid: toUrl(`/memos/${memo.slug}`),
  }));

  const seriesItems: FeedItem[] = seriesList.map((series) => ({
    title: series.title,
    link: toUrl(`/series/${series.slug}`),
    description: series.description ?? `${series.title}の連載ページです`,
    pubDate: series.timeline.updatedAt.toUTCString(),
    guid: toUrl(`/series/${series.slug}`),
  }));

  const chapterItems: FeedItem[] = chaptersBySeries.flatMap(
    ({ series, chapters }) =>
      chapters.map((chapter) => ({
        title: `${chapter.title} | ${series.title}`,
        link: toUrl(`/series/${series.slug}/chapters/${chapter.slug}`),
        description: `${series.title} - ${chapter.title}`,
        pubDate: chapter.timeline.updatedAt.toUTCString(),
        guid: toUrl(`/series/${series.slug}/chapters/${chapter.slug}`),
      })),
  );

  const allItems = [
    ...articleItems,
    ...memoItems,
    ...seriesItems,
    ...chapterItems,
  ].sort(
    (a, b) => new Date(b.pubDate).getTime() - new Date(a.pubDate).getTime(),
  );

  const xml = buildRssXml(siteUrl, allItems);

  return new Response(xml, {
    headers: {
      "Content-Type": "application/rss+xml; charset=utf-8",
      "Cache-Control": "public, max-age=3600, s-maxage=3600",
    },
  });
}
