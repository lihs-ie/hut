/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";

vi.mock("swr", () => ({
  default: vi.fn(),
}));

vi.mock("next/image", () => ({
  default: (imageProps: Record<string, unknown>) => {
    const { src, alt, width, height } = imageProps;
    return (
      <img
        src={typeof src === "string" ? src : ""}
        alt={typeof alt === "string" ? alt : ""}
        width={typeof width === "number" ? width : undefined}
        height={typeof height === "number" ? height : undefined}
      />
    );
  },
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
import { LinkCardClient } from "@shared/components/molecules/card/link.client";

describe("components/molecules/card/LinkCardClient", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("タイトルは h3 ではなく p 要素として描画される", () => {
    vi.mocked(useSWR).mockReturnValue({
      data: {
        url: "https://example.com",
        title: "サンプルタイトル",
        description: "サンプル説明",
        favicon: null,
        image: null,
      },
      isLoading: false,
    } as ReturnType<typeof useSWR>);

    render(<LinkCardClient url="https://example.com" />);

    expect(screen.queryByRole("heading", { level: 3 })).toBeNull();
    expect(screen.getByText("サンプルタイトル").tagName.toLowerCase()).toBe("p");
  });

  it("タイトルテキストが描画される", () => {
    vi.mocked(useSWR).mockReturnValue({
      data: {
        url: "https://example.com",
        title: "別タイトル",
        description: null,
        favicon: null,
        image: null,
      },
      isLoading: false,
    } as ReturnType<typeof useSWR>);

    render(<LinkCardClient url="https://example.com" />);

    expect(screen.getByText("別タイトル")).toBeInTheDocument();
  });
});
