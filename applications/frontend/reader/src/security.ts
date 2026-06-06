const MALICIOUS_USER_AGENT_PATTERNS: ReadonlyArray<RegExp> = [
  // 脆弱性スキャナー
  /sqlmap/i,
  /nikto/i,
  /nmap/i,
  /gobuster/i,
  /\bdirb\b/i,
  /dirbuster/i,
  /wpscan/i,
  /nuclei/i,
  /acunetix/i,
  /netsparker/i,
  /zgrab/i,
  /masscan/i,
  /censys/i,
  // SEO / スクレイピング系 (サイト利益なし)
  /semrushbot/i,
  /ahrefsbot/i,
  /mj12bot/i,
  /petalbot/i,
  /dotbot/i,
  // AI 学習クローラー (サイト利益なし、Firestore 読み取りコストを食う)
  /gptbot/i,
  /chatgpt-user/i,
  /oai-searchbot/i,
  /claudebot/i,
  /claude-web/i,
  /anthropic-ai/i,
  /ccbot/i,
  /perplexitybot/i,
  /bytespider/i,
  /amazonbot/i,
  /cohere-ai/i,
  /diffbot/i,
  /google-extended/i,
];

// allowlist は「rate limit の対象外にする」UA。SEO / シェアプレビュー用途のみに限定し、
// それ以外の bot (DuckDuck / Yandex / Baidu / Apple など) は通常の rate limit を通して
// Firestore 読み取りコストの暴走を防ぐ。
const ALLOWED_SEARCH_BOT_PATTERNS: ReadonlyArray<RegExp> = [
  // SEO クリティカル
  /googlebot/i,
  /bingbot/i,
  // シェアプレビュー (人間のシェア起点なので急激なバーストは起きにくい)
  /facebookexternalhit/i,
  /twitterbot/i,
  /linkedinbot/i,
  /slackbot/i,
];

export const isMaliciousUserAgent = (userAgent: string | null): boolean => {
  if (userAgent === null || userAgent.length === 0) {
    return false;
  }
  return MALICIOUS_USER_AGENT_PATTERNS.some((pattern) =>
    pattern.test(userAgent),
  );
};

export const isAllowedSearchBot = (userAgent: string | null): boolean => {
  if (userAgent === null || userAgent.length === 0) {
    return false;
  }
  return ALLOWED_SEARCH_BOT_PATTERNS.some((pattern) =>
    pattern.test(userAgent),
  );
};

const STATIC_PREFIXES = [
  "/_next/static/",
  "/_next/image",
  "/favicon.ico",
  "/icon.png",
  "/robots.txt",
  "/sitemap.xml",
];

export const isStaticPath = (pathname: string): boolean =>
  STATIC_PREFIXES.some((prefix) => pathname.startsWith(prefix));

export const resolveRateLimitKey = (
  request: Request,
  pathname: string,
): string => {
  const ip = request.headers.get("cf-connecting-ip") ?? "anonymous";
  return `${ip}:${pathname}`;
};
