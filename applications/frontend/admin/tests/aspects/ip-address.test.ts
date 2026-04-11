import { describe, it, expect, vi } from "vitest";
import { resolveIP } from "@/aspects/ip-address";

type MockHeaders = Record<string, string>;

interface MockNextRequest {
  readonly headers: {
    readonly get: (key: string) => string | null;
  };
}

const createMockRequest = (headers: MockHeaders): MockNextRequest => ({
  headers: {
    get: vi.fn((key: string) => headers[key.toLowerCase()] ?? null),
  },
});

describe("aspects/ip-address", () => {
  describe("resolveIP", () => {
    describe("x-forwarded-for ヘッダーがある場合", () => {
      it("単一のIPアドレスからIPを解決する", () => {
        const request = createMockRequest({ "x-forwarded-for": "192.168.1.1" });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:192.168.1.1");
      });

      it("複数のIPアドレスから最右端のIPを解決する（信頼できるプロキシが付与した値）", () => {
        const request = createMockRequest({
          "x-forwarded-for": "192.168.1.1, 10.0.0.1, 172.16.0.1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:172.16.0.1");
      });

      it("先頭と末尾の空白を除去する", () => {
        const request = createMockRequest({
          "x-forwarded-for": "  192.168.1.1  ",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:192.168.1.1");
      });

      it("空文字列の場合は次のヘッダーにフォールバックする", () => {
        const request = createMockRequest({
          "x-forwarded-for": "",
          "x-real-ip": "10.0.0.1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:10.0.0.1");
      });
    });

    describe("IPv4-mapped IPv6 アドレスの正規化", () => {
      it("x-forwarded-for の IPv4-mapped IPv6 を IPv4 に正規化する", () => {
        const request = createMockRequest({
          "x-forwarded-for": "::ffff:192.168.1.1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:192.168.1.1");
      });

      it("x-real-ip の IPv4-mapped IPv6 を IPv4 に正規化する", () => {
        const request = createMockRequest({
          "x-real-ip": "::ffff:10.0.0.1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:10.0.0.1");
      });

      it("大文字の IPv4-mapped IPv6 プレフィックスも正規化する", () => {
        const request = createMockRequest({
          "x-forwarded-for": "::FFFF:192.168.1.1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:192.168.1.1");
      });

      it("純粋な IPv6 アドレスはそのまま返す", () => {
        const request = createMockRequest({
          "x-forwarded-for": "2001:db8::1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:2001:db8::1");
      });
    });

    describe("x-real-ip ヘッダーがある場合", () => {
      it("x-real-ip からIPを解決する", () => {
        const request = createMockRequest({ "x-real-ip": "10.0.0.1" });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:10.0.0.1");
      });

      it("先頭と末尾の空白を除去する", () => {
        const request = createMockRequest({ "x-real-ip": "  10.0.0.1  " });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:10.0.0.1");
      });

      it("空文字列の場合は unknown を返す", () => {
        const request = createMockRequest({
          "x-real-ip": "",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("unknown");
      });
    });

    describe("ヘッダーがない場合", () => {
      it("unknown を返す", () => {
        const request = createMockRequest({});

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("unknown");
      });
    });

    describe("優先順位", () => {
      it("x-forwarded-for が最優先される", () => {
        const request = createMockRequest({
          "x-forwarded-for": "10.0.0.1, 192.168.1.1",
          "x-real-ip": "10.0.0.1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:192.168.1.1");
      });

      it("x-forwarded-for がない場合 x-real-ip が使われる", () => {
        const request = createMockRequest({
          "x-real-ip": "10.0.0.1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:10.0.0.1");
      });

      it("IPヘッダーがない場合 unknown を返す", () => {
        const request = createMockRequest({});

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("unknown");
      });
    });
  });
});
