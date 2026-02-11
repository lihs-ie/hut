/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

const mockFetch = vi.fn();
global.fetch = mockFetch;

vi.mock("next/cache", () => ({
  unstable_cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

/**
 * テスト用のHTMLレスポンスを生成する
 */
function createMockHtmlResponse(html: string) {
  return {
    ok: true,
    text: () => Promise.resolve(html),
  };
}

/**
 * 基本的なOGPメタタグを含むHTMLを生成する
 */
function createOgpHtml(options: {
  ogTitle?: string;
  ogDescription?: string;
  ogImage?: string;
  ogSiteName?: string;
  title?: string;
  metaDescription?: string;
  favicon?: string;
}) {
  const metaTags = [];

  if (options.ogTitle) {
    metaTags.push(`<meta property="og:title" content="${options.ogTitle}" />`);
  }
  if (options.ogDescription) {
    metaTags.push(`<meta property="og:description" content="${options.ogDescription}" />`);
  }
  if (options.ogImage) {
    metaTags.push(`<meta property="og:image" content="${options.ogImage}" />`);
  }
  if (options.ogSiteName) {
    metaTags.push(`<meta property="og:site_name" content="${options.ogSiteName}" />`);
  }
  if (options.metaDescription) {
    metaTags.push(`<meta name="description" content="${options.metaDescription}" />`);
  }

  const faviconLink = options.favicon
    ? `<link rel="icon" href="${options.favicon}" />`
    : "";

  const titleTag = options.title ? `<title>${options.title}</title>` : "";

  return `
    <!DOCTYPE html>
    <html>
      <head>
        ${titleTag}
        ${metaTags.join("\n        ")}
        ${faviconLink}
      </head>
      <body></body>
    </html>
  `;
}

describe("ogp actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockFetch.mockReset();
  });

  describe("fetchOGP", () => {
    it("OGPメタデータを正常に取得できる", async () => {
      const html = createOgpHtml({
        ogTitle: "OG Test Title",
        ogDescription: "OG Test Description",
        ogImage: "https://example.com/image.png",
        ogSiteName: "Test Site",
        title: "Test Page",
        favicon: "/favicon.ico",
      });
      mockFetch.mockResolvedValueOnce(createMockHtmlResponse(html));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com");

      expect(result).toEqual({
        url: "https://example.com",
        title: "OG Test Title",
        description: "OG Test Description",
        image: "https://example.com/image.png",
        siteName: "Test Site",
        favicon: "https://example.com/favicon.ico",
      });
    });

    it("og:titleがない場合はtitleタグを使用する", async () => {
      const html = createOgpHtml({
        title: "Fallback Title",
        metaDescription: "Meta Description",
      });
      mockFetch.mockResolvedValueOnce(createMockHtmlResponse(html));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com");

      expect(result.title).toBe("Fallback Title");
      expect(result.description).toBe("Meta Description");
    });

    it("HTTPエラーの場合はURLのみを返す", async () => {
      mockFetch.mockResolvedValueOnce({ ok: false, status: 404 });

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com/not-found");

      expect(result).toEqual({ url: "https://example.com/not-found" });
    });

    it("ネットワークエラーの場合はURLのみを返す", async () => {
      mockFetch.mockRejectedValueOnce(new Error("Network error"));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com");

      expect(result).toEqual({ url: "https://example.com" });
    });

    it("無効なプロトコルの場合はURLのみを返す", async () => {
      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("ftp://example.com");

      expect(result).toEqual({ url: "ftp://example.com" });
    });

    it("相対URLの画像パスを解決する", async () => {
      const html = createOgpHtml({ ogImage: "/images/og.png" });
      mockFetch.mockResolvedValueOnce(createMockHtmlResponse(html));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com/page");

      expect(result.image).toBe("https://example.com/images/og.png");
    });

    it("プロトコル相対URLを解決する", async () => {
      const html = createOgpHtml({ ogImage: "//cdn.example.com/image.png" });
      mockFetch.mockResolvedValueOnce(createMockHtmlResponse(html));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com");

      expect(result.image).toBe("https://cdn.example.com/image.png");
    });

    it("HTMLエンティティをデコードする", async () => {
      const html = `
        <!DOCTYPE html>
        <html>
          <head>
            <meta property="og:title" content="Test &amp; Title" />
            <meta property="og:description" content="Quote: &quot;Hello&quot;" />
          </head>
          <body></body>
        </html>
      `;
      mockFetch.mockResolvedValueOnce(createMockHtmlResponse(html));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com");

      expect(result.title).toBe("Test & Title");
      expect(result.description).toBe('Quote: "Hello"');
    });

    it("faviconがない場合はデフォルトパスを使用する", async () => {
      const html = createOgpHtml({ title: "No Favicon" });
      mockFetch.mockResolvedValueOnce(createMockHtmlResponse(html));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com/page");

      expect(result.favicon).toBe("https://example.com/favicon.ico");
    });

    it("name属性のdescriptionメタタグも検出する", async () => {
      const html = createOgpHtml({ metaDescription: "Name Description" });
      mockFetch.mockResolvedValueOnce(createMockHtmlResponse(html));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com");

      expect(result.description).toBe("Name Description");
    });

    it("content属性が先に来るメタタグも検出する", async () => {
      const html = `
        <!DOCTYPE html>
        <html>
          <head>
            <meta content="Reverse Order Title" property="og:title" />
          </head>
          <body></body>
        </html>
      `;
      mockFetch.mockResolvedValueOnce(createMockHtmlResponse(html));

      const { fetchOGP } = await import("@shared/actions/ogp");
      const result = await fetchOGP("https://example.com");

      expect(result.title).toBe("Reverse Order Title");
    });
  });
});
