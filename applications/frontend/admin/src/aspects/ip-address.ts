import { NextRequest } from "next/server";

export const resolveIP = (request: NextRequest): string => {
  const forwardedFor = request.headers.get("x-forwarded-for");
  const forwardedIp = forwardedFor?.split(",")[0]?.trim();

  if (forwardedIp) {
    return `ip:${forwardedIp}`;
  }

  const realIp = request.headers.get("x-real-ip")?.trim();

  if (realIp) {
    return `ip:${realIp}`;
  }

  const userAgent = request.headers.get("user-agent")?.trim();

  if (userAgent) {
    return `ua:${userAgent}`;
  }

  return "unknown";
};
