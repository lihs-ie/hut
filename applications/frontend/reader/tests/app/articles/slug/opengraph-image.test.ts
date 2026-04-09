/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../../shared/tests/support/molds/domains/article/common";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@/actions/article", () => ({
  findBySlug: vi.fn(),
}));

vi.mock("next/og", () => {
  const ImageResponse = vi.fn().mockImplementation(function MockImageResponse() {
    return new Response("image", { headers: { "Content-Type": "image/png" } });
  });

  Object.setPrototypeOf(ImageResponse.prototype, Response.prototype);

  return { ImageResponse };
});

describe("articles/[slug]/opengraph-image", () => {
  beforeEach(() => {
    vi.resetModules();
  });

  it("size が定義されている", async () => {
    const { size } = await import(
      "../../../../src/app/articles/[slug]/opengraph-image"
    );

    expect(size).toBeDefined();
    expect(size.width).toBeGreaterThan(0);
    expect(size.height).toBeGreaterThan(0);
  });

  it("contentType が image/png である", async () => {
    const { contentType } = await import(
      "../../../../src/app/articles/[slug]/opengraph-image"
    );

    expect(contentType).toBe("image/png");
  });

  it("記事タイトルを使ってImageResponseを生成する", async () => {
    const article = Forger(ArticleMold).forgeWithSeed(1);
    const { findBySlug } = await import("@/actions/article");
    vi.mocked(findBySlug).mockResolvedValue(article);

    const { default: Image } = await import(
      "../../../../src/app/articles/[slug]/opengraph-image"
    );

    const { ImageResponse } = await import("next/og");
    await Image({ params: Promise.resolve({ slug: article.slug }) });

    expect(vi.mocked(ImageResponse)).toHaveBeenCalled();
    const callArg = vi.mocked(ImageResponse).mock.calls[0][0];
    expect(JSON.stringify(callArg)).toContain(article.title);
  });
});
