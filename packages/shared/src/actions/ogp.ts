"use server";

import { unstable_cache } from "next/cache";

export type OGPMetadata = {
  url: string;
  title?: string;
  description?: string;
  image?: string;
  siteName?: string;
  favicon?: string;
};

const extractMetaContent = (
  html: string,
  property: string,
): string | undefined => {
  const patterns = [
    new RegExp(
      `<meta[^>]*property=["']${property}["'][^>]*content=["']([^"']*)["']`,
      "i",
    ),
    new RegExp(
      `<meta[^>]*content=["']([^"']*)["'][^>]*property=["']${property}["']`,
      "i",
    ),
    new RegExp(
      `<meta[^>]*name=["']${property}["'][^>]*content=["']([^"']*)["']`,
      "i",
    ),
    new RegExp(
      `<meta[^>]*content=["']([^"']*)["'][^>]*name=["']${property}["']`,
      "i",
    ),
  ];

  for (const pattern of patterns) {
    const match = html.match(pattern);
    if (match?.[1]) {
      return decodeHTMLEntities(match[1]);
    }
  }

  return undefined;
};

const extractTitle = (html: string): string | undefined => {
  const match = html.match(/<title[^>]*>([^<]*)<\/title>/i);
  return match?.[1] ? decodeHTMLEntities(match[1].trim()) : undefined;
};

const extractFavicon = (html: string, baseUrl: string): string | undefined => {
  const patterns = [
    /<link[^>]*rel=["'](?:shortcut )?icon["'][^>]*href=["']([^"']*)["']/i,
    /<link[^>]*href=["']([^"']*)["'][^>]*rel=["'](?:shortcut )?icon["']/i,
  ];

  for (const pattern of patterns) {
    const match = html.match(pattern);
    if (match?.[1]) {
      return resolveUrl(match[1], baseUrl);
    }
  }

  return `${new URL(baseUrl).origin}/favicon.ico`;
};

const resolveUrl = (url: string, baseUrl: string): string => {
  if (url.startsWith("http://") || url.startsWith("https://")) {
    return url;
  }
  if (url.startsWith("//")) {
    return `https:${url}`;
  }
  const base = new URL(baseUrl);
  if (url.startsWith("/")) {
    return `${base.origin}${url}`;
  }
  return `${base.origin}/${url}`;
};

const decodeHTMLEntities = (text: string): string => {
  const entities: Record<string, string> = {
    "&amp;": "&",
    "&lt;": "<",
    "&gt;": ">",
    "&quot;": '"',
    "&#39;": "'",
    "&apos;": "'",
    "&nbsp;": " ",
  };

  let decoded = text;
  for (const [entity, char] of Object.entries(entities)) {
    decoded = decoded.replace(new RegExp(entity, "g"), char);
  }

  decoded = decoded.replace(/&#(\d+);/g, (_, code) =>
    String.fromCharCode(parseInt(code, 10)),
  );
  decoded = decoded.replace(/&#x([0-9a-fA-F]+);/g, (_, code) =>
    String.fromCharCode(parseInt(code, 16)),
  );

  return decoded;
};

const fetchOGPInternal = async (url: string): Promise<OGPMetadata> => {
  try {
    const parsedUrl = new URL(url);
    if (!["http:", "https:"].includes(parsedUrl.protocol)) {
      return { url };
    }

    const response = await fetch(url, {
      headers: {
        "User-Agent":
          "Mozilla/5.0 (compatible; OGPBot/1.0; +https://example.com)",
        Accept: "text/html,application/xhtml+xml",
      },
      signal: AbortSignal.timeout(5000),
    });

    if (!response.ok) {
      return { url };
    }

    const html = await response.text();

    const ogTitle = extractMetaContent(html, "og:title");
    const ogDescription = extractMetaContent(html, "og:description");
    const ogImage = extractMetaContent(html, "og:image");
    const ogSiteName = extractMetaContent(html, "og:site_name");

    const title = ogTitle || extractTitle(html);
    const description =
      ogDescription || extractMetaContent(html, "description");
    const image = ogImage ? resolveUrl(ogImage, url) : undefined;
    const favicon = extractFavicon(html, url);

    return {
      url,
      title,
      description,
      image,
      siteName: ogSiteName,
      favicon,
    };
  } catch {
    return { url };
  }
};

export const fetchOGP = async (url: string): Promise<OGPMetadata> =>
  unstable_cache(() => fetchOGPInternal(url), ["ogp", url], {
    revalidate: 86400,
  })();
