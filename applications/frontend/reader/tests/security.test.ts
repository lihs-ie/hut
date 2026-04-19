import { describe, expect, it } from "vitest";
import {
  isAllowedSearchBot,
  isMaliciousUserAgent,
  isStaticPath,
  resolveRateLimitKey,
} from "@/security";

describe("security", () => {
  describe("isMaliciousUserAgent", () => {
    it.each([
      "sqlmap/1.7.5#stable (http://sqlmap.org)",
      "Mozilla/5.0 (compatible; Nikto/2.5.0)",
      "gobuster/3.6",
      "WPScan v3.8.27",
      "Nuclei - Open-source project",
      "SemrushBot/7~bl",
      "AhrefsBot/7.0",
      "Mozilla/5.0 AppleWebKit/537.36 (compatible; GPTBot/1.2; +https://openai.com/gptbot)",
      "Mozilla/5.0 (compatible; ChatGPT-User/1.0; +https://openai.com/bot)",
      "Mozilla/5.0 (compatible; OAI-SearchBot/1.0; +https://openai.com/searchbot)",
      "Mozilla/5.0 (compatible; ClaudeBot/1.0; +claudebot@anthropic.com)",
      "Mozilla/5.0 (compatible; anthropic-ai/1.0)",
      "CCBot/2.0 (https://commoncrawl.org/faq/)",
      "Mozilla/5.0 (compatible; PerplexityBot/1.0; +https://perplexity.ai/bot)",
      "Mozilla/5.0 (compatible; Bytespider; spider-feedback@bytedance.com)",
      "Mozilla/5.0 (compatible; Amazonbot/0.1; +https://developer.amazon.com/amazonbot)",
    ])("returns true for %s", (userAgent) => {
      expect(isMaliciousUserAgent(userAgent)).toBe(true);
    });

    it.each([
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
      "Googlebot/2.1 (+http://www.google.com/bot.html)",
      null,
      "",
    ])("returns false for %s", (userAgent) => {
      expect(isMaliciousUserAgent(userAgent)).toBe(false);
    });
  });

  describe("isAllowedSearchBot", () => {
    it.each([
      "Googlebot/2.1 (+http://www.google.com/bot.html)",
      "Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)",
      "facebookexternalhit/1.1",
      "Twitterbot/1.0",
      "LinkedInBot/1.0 (compatible; Mozilla/5.0)",
      "Slackbot-LinkExpanding 1.0",
    ])("returns true for %s", (userAgent) => {
      expect(isAllowedSearchBot(userAgent)).toBe(true);
    });

    it.each([
      "DuckDuckBot/1.1; (+http://duckduckgo.com/duckduckbot.html)",
      "Mozilla/5.0 (compatible; YandexBot/3.0; +http://yandex.com/bots)",
      "Mozilla/5.0 (compatible; Baiduspider/2.0; +http://www.baidu.com/search/spider.html)",
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleBot/0.1",
    ])(
      "returns false for low-value bot %s (rate limit 対象に戻す)",
      (userAgent) => {
        expect(isAllowedSearchBot(userAgent)).toBe(false);
      },
    );

    it("returns false for a regular browser user agent", () => {
      expect(
        isAllowedSearchBot(
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/127",
        ),
      ).toBe(false);
    });

    it("returns false for malicious scanners", () => {
      expect(isAllowedSearchBot("sqlmap/1.7.5")).toBe(false);
    });

    it("returns false for AI crawlers (now on the blacklist)", () => {
      expect(isAllowedSearchBot("GPTBot/1.2")).toBe(false);
    });

    it.each([null, ""])("returns false for %s", (userAgent) => {
      expect(isAllowedSearchBot(userAgent)).toBe(false);
    });
  });

  describe("isStaticPath", () => {
    it.each([
      "/_next/static/chunks/main.js",
      "/_next/image",
      "/favicon.ico",
      "/icon.png",
      "/robots.txt",
      "/sitemap.xml",
    ])("returns true for %s", (pathname) => {
      expect(isStaticPath(pathname)).toBe(true);
    });

    it.each(["/articles/foo", "/api/engagement", "/"])(
      "returns false for %s",
      (pathname) => {
        expect(isStaticPath(pathname)).toBe(false);
      },
    );
  });

  describe("resolveRateLimitKey", () => {
    it("composes key from cf-connecting-ip and pathname", () => {
      const request = new Request("https://example.com/articles/foo", {
        headers: { "cf-connecting-ip": "203.0.113.10" },
      });
      expect(resolveRateLimitKey(request, "/articles/foo")).toBe(
        "203.0.113.10:/articles/foo",
      );
    });

    it("falls back to anonymous when cf-connecting-ip is missing", () => {
      const request = new Request("https://example.com/");
      expect(resolveRateLimitKey(request, "/")).toBe("anonymous:/");
    });
  });
});
