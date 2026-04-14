/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { createAccessTokenProvider } from "@/infrastructures/firestore-rest/access-token";

const TEST_PRIVATE_KEY = `-----BEGIN PRIVATE KEY-----
MIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQDCLhvFjeNn0YZq
WcaFT4xKl0RhSsSJhd3/8idHTwt8TFyjUn5AROnbmdBSKjuF7CXSaftk0yEh4aeo
Em3mM5Sx8FLT/GWRhOJnXR65T4Z5iQZiMxkr/Snm9zI7yohn7WnqCOAHUeMI2Id1
zkg7zqr7LY13wPzQ5QCrp9j+SbeXKE9AbCjs0JRIRRX0EVDU70PuVBbimx7KFLkK
PqgF+vzf97lHLzFoMJ45qlxm/RhfHtnZw9Cas7W0ymAncUQ8KLahEYjyMbX+rw6M
tOfbNg/Dxe55XDGQRTAj3k/7QDfPHfDM/p3U1fQS8FlIFVpjiI6Fv0ze7tkT0qVD
3wr8H37dAgMBAAECggEABjx5g5YxBYmJjMfBgOCGxX0wfGtJXPnvijNFHqPiKKHj
hdcUvkmLT5v3ZjnUEYShJILzZyBNpb4EwSitUcj+lSKNVYnZRqcpAZbVI8Lvkaih
EjlNeF99vSCSMLjRTl6jnMgPKr4g3ROgW9RK8POZWhQWddSe8kGLpCWhJFywcDZn
lGsfNWoX09Gd2u/mC6GYxaeawIGDN81LOcGEipvYX5eTdSuBmL7wOxSZ2EZwMXUL
6lyVXrO6+gG4GjXQcnJa9T8XTPCnaHjOPTIZcwb/hufx2d4+Fu4QoTsirmiFiXjY
SqKOsFi6f0Q/56GspVp4Et1z6uHxd6S67CfLH8Z8AQKBgQD1VlP/N8n3tZVQCrCt
aBsdiO1xDMkvAiyOGlXI9BCUvy+a4DEvnEkUWbWUx8GPR/TtvSoEIe/F1MJdtc8z
xyqjFtVfC1CrOK+hXVkxKkwOQM0HlA0aKm//rj3WR5UWBFP4PJm6KGHFhQWdnaxP
6vxaOxIgZEEkYBu0WARrSc/1gQKBgQDKo5kW/svx6KDmgqMRM+o4hMCrx8HhTSel
Wkec8l2KoOcMYW/oZkNK7BoDCaGYQZJiN4lTAEAyPGRS1G/uaN4JCUxZzDwVCl7F
4ibMfHawAA6tr3GoG0mCcNK9LEFrmCjpFHTMuLiLQeqmR+xA+RhzwXk6P1+u7Zzr
JflAnxL03QKBgHwoM+GzBfHVjZsnKyzMjDHFifUukyA5xqrY8zGhcUBz4T7kAGuE
E4CvhxROwIx3Bx+6wqkZhRYebrpsZiFgkTMsXBKsgpqc+Kb+JPaqqjQ1L80Q1L1o
hRKagxwGFs0Af63j3wcGn5EZcg62tZlnJs7wJZd+vzyVrd4GmoLNSwABAoGABaSY
yHQO1+4kq7JRmLgpuBCAshDQ8GXV2IQJf5uMdsaH0uohIobsqMlxnp/Zr1DPsAN7
Fra+3Wva/vLLV6fwcbArdYR4awypP3tVFaHf1U8YP2rYUDamL08UX9KEK9ym0ZyC
hFQk5YKuVWs7hg/wDEoLmJj2Wq3rHS+Q8FO5yxUCgYBrMIW9RKHUOk+rOwFWYvPP
g1dXvbO8p/PVkYU7t9w98PD+afwxXKMptvOQNE4pIKAeKE42lM5o94COBIaH++Gq
hzhOrvxt6CRjCnLsFwvw4FEHgGVmCMd+jiYU1UAaofRiIqD2Bv5I0KFSBMUk+zZ5
aXfm+pxkgQH1B4QCHS12dg==
-----END PRIVATE KEY-----`;

const createMockResponse = (body: unknown, status: number = 200): Response => {
  return new Response(JSON.stringify(body), {
    status,
    headers: { "Content-Type": "application/json" },
  });
};

describe("infrastructures/firestore-rest/access-token", () => {
  let originalFetch: typeof fetch;
  let originalDateNow: typeof Date.now;

  beforeEach(() => {
    originalFetch = globalThis.fetch;
    originalDateNow = Date.now;
    Date.now = () => 1_700_000_000_000;
  });

  afterEach(() => {
    globalThis.fetch = originalFetch;
    Date.now = originalDateNow;
  });

  it("OAuth2 エンドポイントへアクセストークンを要求する", async () => {
    const mockFetch = vi.fn().mockResolvedValue(
      createMockResponse({
        access_token: "test-access-token",
        expires_in: 3600,
        token_type: "Bearer",
      }),
    );
    globalThis.fetch = mockFetch;

    const provider = createAccessTokenProvider({
      clientEmail: "service@demo-hut.iam.gserviceaccount.com",
      privateKey: TEST_PRIVATE_KEY,
    });

    const token = await provider.getAccessToken();

    expect(token).toBe("test-access-token");
    expect(mockFetch).toHaveBeenCalledWith(
      "https://oauth2.googleapis.com/token",
      expect.objectContaining({
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
      }),
    );
  });

  it("取得したトークンを有効期限内ではキャッシュから返す", async () => {
    const mockFetch = vi.fn().mockResolvedValue(
      createMockResponse({
        access_token: "cached-token",
        expires_in: 3600,
        token_type: "Bearer",
      }),
    );
    globalThis.fetch = mockFetch;

    const provider = createAccessTokenProvider({
      clientEmail: "service@demo-hut.iam.gserviceaccount.com",
      privateKey: TEST_PRIVATE_KEY,
    });

    const first = await provider.getAccessToken();
    const second = await provider.getAccessToken();

    expect(first).toBe("cached-token");
    expect(second).toBe("cached-token");
    expect(mockFetch).toHaveBeenCalledTimes(1);
  });

  it("有効期限を超過した場合はトークンを再取得する", async () => {
    const mockFetch = vi
      .fn()
      .mockResolvedValueOnce(
        createMockResponse({
          access_token: "first-token",
          expires_in: 60,
          token_type: "Bearer",
        }),
      )
      .mockResolvedValueOnce(
        createMockResponse({
          access_token: "second-token",
          expires_in: 3600,
          token_type: "Bearer",
        }),
      );
    globalThis.fetch = mockFetch;

    const provider = createAccessTokenProvider({
      clientEmail: "service@demo-hut.iam.gserviceaccount.com",
      privateKey: TEST_PRIVATE_KEY,
    });

    const first = await provider.getAccessToken();
    Date.now = () => 1_700_000_000_000 + 120 * 1000;
    const second = await provider.getAccessToken();

    expect(first).toBe("first-token");
    expect(second).toBe("second-token");
    expect(mockFetch).toHaveBeenCalledTimes(2);
  });

  it("401 レスポンスの場合はエラーをスローする", async () => {
    const mockFetch = vi
      .fn()
      .mockResolvedValue(createMockResponse({ error: "invalid_grant" }, 401));
    globalThis.fetch = mockFetch;

    const provider = createAccessTokenProvider({
      clientEmail: "service@demo-hut.iam.gserviceaccount.com",
      privateKey: TEST_PRIVATE_KEY,
    });

    await expect(provider.getAccessToken()).rejects.toThrow();
  });

  it("アクセストークンが応答に含まれない場合はエラーをスローする", async () => {
    const mockFetch = vi
      .fn()
      .mockResolvedValue(createMockResponse({ expires_in: 3600 }));
    globalThis.fetch = mockFetch;

    const provider = createAccessTokenProvider({
      clientEmail: "service@demo-hut.iam.gserviceaccount.com",
      privateKey: TEST_PRIVATE_KEY,
    });

    await expect(provider.getAccessToken()).rejects.toThrow();
  });
});
