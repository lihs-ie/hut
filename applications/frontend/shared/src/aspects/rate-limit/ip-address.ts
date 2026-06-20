import { NextRequest } from "next/server";

const IPV4_MAPPED_IPV6_PREFIX = "::ffff:";

const normalizeIPv4MappedIPv6 = (ip: string): string => {
  if (ip.toLowerCase().startsWith(IPV4_MAPPED_IPV6_PREFIX)) {
    return ip.slice(IPV4_MAPPED_IPV6_PREFIX.length);
  }

  return ip;
};

const readSingleHeader = (
  request: NextRequest,
  name: string,
): string | null => {
  const value = request.headers.get(name)?.trim();
  return value && value.length > 0 ? value : null;
};

export const resolveIP = (request: NextRequest): string => {
  const cloudflareIp = readSingleHeader(request, "cf-connecting-ip");

  if (cloudflareIp) {
    return `ip:${normalizeIPv4MappedIPv6(cloudflareIp)}`;
  }

  const trueClientIp = readSingleHeader(request, "true-client-ip");

  if (trueClientIp) {
    return `ip:${normalizeIPv4MappedIPv6(trueClientIp)}`;
  }

  const forwardedFor = request.headers.get("x-forwarded-for");

  if (forwardedFor) {
    const ips = forwardedFor
      .split(",")
      .map((ip) => ip.trim())
      .filter((ip) => ip.length > 0);
    const rightmostIp = ips[ips.length - 1];

    if (rightmostIp) {
      return `ip:${normalizeIPv4MappedIPv6(rightmostIp)}`;
    }
  }

  const realIp = readSingleHeader(request, "x-real-ip");

  if (realIp) {
    return `ip:${normalizeIPv4MappedIPv6(realIp)}`;
  }

  return "ip:unknown";
};
