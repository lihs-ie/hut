const MALICIOUS_USER_AGENT_PATTERNS: ReadonlyArray<RegExp> = [
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
  /semrushbot/i,
  /ahrefsbot/i,
  /mj12bot/i,
  /petalbot/i,
  /dotbot/i,
];

const ALLOWED_SEARCH_BOT_PATTERNS: ReadonlyArray<RegExp> = [
  /googlebot/i,
  /bingbot/i,
  /duckduckbot/i,
  /yandexbot/i,
  /baiduspider/i,
  /applebot/i,
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
