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

      it("複数のIPアドレスから最初のIPを解決する", () => {
        const request = createMockRequest({
          "x-forwarded-for": "192.168.1.1, 10.0.0.1, 172.16.0.1",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:192.168.1.1");
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

      it("空文字列の場合は次のヘッダーにフォールバックする", () => {
        const request = createMockRequest({
          "x-real-ip": "",
          "user-agent": "Mozilla/5.0",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ua:Mozilla/5.0");
      });
    });

    describe("user-agent ヘッダーがある場合", () => {
      it("user-agent から識別子を解決する", () => {
        const request = createMockRequest({
          "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ua:Mozilla/5.0 (Windows NT 10.0; Win64; x64)");
      });

      it("先頭と末尾の空白を除去する", () => {
        const request = createMockRequest({ "user-agent": "  Mozilla/5.0  " });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ua:Mozilla/5.0");
      });

      it("空文字列の場合は unknown を返す", () => {
        const request = createMockRequest({ "user-agent": "" });

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
          "x-forwarded-for": "192.168.1.1",
          "x-real-ip": "10.0.0.1",
          "user-agent": "Mozilla/5.0",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:192.168.1.1");
      });

      it("x-forwarded-for がない場合 x-real-ip が使われる", () => {
        const request = createMockRequest({
          "x-real-ip": "10.0.0.1",
          "user-agent": "Mozilla/5.0",
        });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ip:10.0.0.1");
      });

      it("IPヘッダーがない場合 user-agent が使われる", () => {
        const request = createMockRequest({ "user-agent": "Mozilla/5.0" });

        const result = resolveIP(request as Parameters<typeof resolveIP>[0]);

        expect(result).toBe("ua:Mozilla/5.0");
      });
    });
  });
});
