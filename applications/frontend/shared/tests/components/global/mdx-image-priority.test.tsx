/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render } from "@testing-library/react";
import React from "react";

vi.mock("next/image", () => ({
  __esModule: true,
  default: (imageProps: Record<string, unknown>) => {
    const { priority, ...rest } = imageProps;
    const extraProps = priority ? { fetchpriority: "high" } : {};
    return (
      <img
        {...(rest as React.ImgHTMLAttributes<HTMLImageElement>)}
        {...(extraProps as React.ImgHTMLAttributes<HTMLImageElement>)}
      />
    );
  },
}));

vi.mock("next-mdx-remote/rsc", () => ({
  MDXRemote: (mdxProps: {
    components?: Record<string, React.ComponentType<Record<string, unknown>>>;
  }) => {
    const ImgComponent = mdxProps.components?.img;
    if (!ImgComponent) return null;
    return (
      <div>
        <ImgComponent src="/first.png" alt="first" />
        <ImgComponent src="/second.png" alt="second" />
      </div>
    );
  },
}));

vi.mock("@shikijs/rehype", () => ({ default: vi.fn() }));
vi.mock("remark-gfm", () => ({ default: vi.fn() }));
vi.mock("remark-breaks", () => ({ default: vi.fn() }));
vi.mock("rehype-slug", () => ({ default: vi.fn() }));
vi.mock("@shared/plugins/remark-link-card", () => ({
  remarkLinkCard: vi.fn(),
}));
vi.mock("@shared/components/molecules/card/link", () => ({
  LinkCard: vi.fn(),
}));
vi.mock("@shared/plugins/remark-mermaid", () => ({
  remarkMermaid: vi.fn(),
}));
vi.mock("@shared/components/molecules/button/copy", () => ({
  CopyButton: vi.fn(),
}));
vi.mock("@shared/components/global/mdx.module.css", () => ({
  default: { container: "container", pre: "pre", copy: "copy" },
}));

describe("MDXRenderer - 画像 priority", () => {
  it("最初の img は priority が付く", async () => {
    const { MDXRenderer } = await import("@shared/components/global/mdx");
    const { container } = render(<>{MDXRenderer("dummy content")}</>);
    const images = container.querySelectorAll("img");

    expect(images[0]).toHaveAttribute("fetchpriority", "high");
  });

  it("2枚目以降の img は priority が付かない", async () => {
    const { MDXRenderer } = await import("@shared/components/global/mdx");
    const { container } = render(<>{MDXRenderer("dummy content")}</>);
    const images = container.querySelectorAll("img");

    expect(images[1]).not.toHaveAttribute("fetchpriority", "high");
  });

  it("2回 render しても各 render で先頭 img に priority が付く", async () => {
    const { MDXRenderer } = await import("@shared/components/global/mdx");
    const { container: firstContainer } = render(
      <>{MDXRenderer("dummy content")}</>
    );
    const { container: secondContainer } = render(
      <>{MDXRenderer("dummy content")}</>
    );

    const firstImages = firstContainer.querySelectorAll("img");
    const secondImages = secondContainer.querySelectorAll("img");

    expect(firstImages[0]).toHaveAttribute("fetchpriority", "high");
    expect(secondImages[0]).toHaveAttribute("fetchpriority", "high");
  });
});
