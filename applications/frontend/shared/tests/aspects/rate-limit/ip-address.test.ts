/**
 * @vitest-environment node
 */
import { describe, it, expect } from "vitest";
import { NextRequest } from "next/server";
import { resolveIP } from "@shared/aspects/rate-limit/ip-address";

const createRequest = (headers: Record<string, string> = {}): NextRequest => {
  const request = new NextRequest("https://example.com/");
  Object.entries(headers).forEach(([key, value]) => {
    request.headers.set(key, value);
  });
  return request;
};

describe("resolveIP", () => {
  it("x-forwarded-for ヘッダーの末尾(最右)のIPを返す", () => {
    const request = createRequest({
      "x-forwarded-for": "10.0.0.1, 10.0.0.2, 192.168.1.1",
    });

    const result = resolveIP(request);

    expect(result).toBe("ip:192.168.1.1");
  });

  it("x-forwarded-for が単一のIPの場合はそのIPを返す", () => {
    const request = createRequest({
      "x-forwarded-for": "203.0.113.1",
    });

    const result = resolveIP(request);

    expect(result).toBe("ip:203.0.113.1");
  });

  it("x-forwarded-for の複数IPからスペースをトリムして返す", () => {
    const request = createRequest({
      "x-forwarded-for": "  10.0.0.1  ,  192.168.1.1  ",
    });

    const result = resolveIP(request);

    expect(result).toBe("ip:192.168.1.1");
  });

  it("IPv4-mapped IPv6アドレスを正規化してIPv4形式で返す", () => {
    const request = createRequest({
      "x-forwarded-for": "::ffff:192.168.1.1",
    });

    const result = resolveIP(request);

    expect(result).toBe("ip:192.168.1.1");
  });

  it("x-forwarded-for がない場合は x-real-ip を返す", () => {
    const request = createRequest({
      "x-real-ip": "10.0.0.1",
    });

    const result = resolveIP(request);

    expect(result).toBe("ip:10.0.0.1");
  });

  it("x-real-ip の IPv4-mapped IPv6 を正規化する", () => {
    const request = createRequest({
      "x-real-ip": "::ffff:10.0.0.1",
    });

    const result = resolveIP(request);

    expect(result).toBe("ip:10.0.0.1");
  });

  it("x-forwarded-for も x-real-ip もない場合は unknown を返す", () => {
    const request = createRequest({});

    const result = resolveIP(request);

    expect(result).toBe("unknown");
  });
});
