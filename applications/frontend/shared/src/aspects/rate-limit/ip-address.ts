import { NextRequest } from "next/server";

const IPV4_MAPPED_IPV6_PREFIX = "::ffff:";

const normalizeIPv4MappedIPv6 = (ip: string): string => {
  if (ip.toLowerCase().startsWith(IPV4_MAPPED_IPV6_PREFIX)) {
    return ip.slice(IPV4_MAPPED_IPV6_PREFIX.length);
  }

  return ip;
};

export const resolveIP = (request: NextRequest): string => {
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

  const realIp = request.headers.get("x-real-ip")?.trim();

  if (realIp) {
    return `ip:${normalizeIPv4MappedIPv6(realIp)}`;
  }

  return "unknown";
};
