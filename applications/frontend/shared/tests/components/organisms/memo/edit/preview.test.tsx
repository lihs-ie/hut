/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";

vi.mock("swr", () => ({
  default: vi.fn(() => ({ data: undefined, isLoading: true })),
}));

vi.mock("next/image", () => ({
  default: (imageProps: Record<string, unknown>) => <img {...imageProps} />,
}));

vi.mock("next/link", () => ({
  default: ({
    children,
    href,
  }: {
    children: React.ReactNode;
    href: string;
  }) => <a href={href}>{children}</a>,
}));

import useSWR from "swr";

describe("components/organisms/memo/edit/EntryPreview", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.mocked(useSWR).mockReturnValue({
      data: undefined,
      isLoading: true,
    } as ReturnType<typeof useSWR>);
  });

  it("URLのみの段落がある場合にLinkCardClientがレンダリングされる", async () => {
    const { EntryPreview } = await import(
      "@shared/components/organisms/memo/edit/preview"
    );

    render(<EntryPreview value="https://example.com" />);

    expect(useSWR).toHaveBeenCalledWith(
      "https://example.com",
      expect.any(Function),
      expect.any(Object),
    );
  });

  it("URLのみの段落はp要素ではなくLinkCardClientとしてレンダリングされる", async () => {
    const { EntryPreview } = await import(
      "@shared/components/organisms/memo/edit/preview"
    );

    const { container } = render(
      <EntryPreview value="https://example.com" />,
    );

    const paragraphs = container.querySelectorAll("p");
    const urlParagraphs = Array.from(paragraphs).filter(
      (paragraph) => paragraph.textContent?.trim() === "https://example.com",
    );
    expect(urlParagraphs).toHaveLength(0);
  });

  it("URLではないテキストの段落は通常のp要素としてレンダリングされる", async () => {
    const { EntryPreview } = await import(
      "@shared/components/organisms/memo/edit/preview"
    );

    render(<EntryPreview value="通常のテキスト段落" />);

    expect(screen.getByText("通常のテキスト段落")).toBeInTheDocument();
    const paragraphElement = screen.getByText("通常のテキスト段落").closest("p");
    expect(paragraphElement).toBeInTheDocument();
  });

  it("複数の子要素を持つ段落は通常のp要素としてレンダリングされる", async () => {
    const { EntryPreview } = await import(
      "@shared/components/organisms/memo/edit/preview"
    );

    render(<EntryPreview value="テキストと **太字** の混在" />);

    expect(useSWR).not.toHaveBeenCalled();
  });
});
