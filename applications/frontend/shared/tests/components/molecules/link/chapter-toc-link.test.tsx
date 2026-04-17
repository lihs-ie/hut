/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";

const mockUsePathname = vi.fn<() => string>();

vi.mock("next/navigation", () => ({
  usePathname: () => mockUsePathname(),
}));

type NavigableLinkProps = {
  href: string;
  children: React.ReactNode;
  className?: string;
  "aria-current"?: "page" | "step" | "location" | "date" | "time" | "true" | "false";
};

vi.mock("@shared/components/molecules/link/navigable", () => ({
  NavigableLink: (linkProps: NavigableLinkProps) => (
    <a
      href={linkProps.href}
      className={linkProps.className}
      aria-current={linkProps["aria-current"]}
    >
      {linkProps.children}
    </a>
  ),
}));

describe("components/molecules/link/ChapterTocLink", () => {
  beforeEach(() => {
    mockUsePathname.mockReset();
  });

  it("現在のパスと一致する場合は aria-current=page が付与される", async () => {
    mockUsePathname.mockReturnValue("/series/foo/chapters/bar");

    const { ChapterTocLink } = await import(
      "@shared/components/molecules/link/chapter-toc-link"
    );

    const { unmount } = render(
      <ChapterTocLink
        href="/series/foo/chapters/bar"
        className="inactive"
        activeClassName="active"
      >
        チャプタータイトル
      </ChapterTocLink>,
    );

    const link = screen.getByText("チャプタータイトル");
    expect(link).toHaveAttribute("aria-current", "page");

    unmount();
  });

  it("現在のパスと異なる場合は aria-current が付与されない", async () => {
    mockUsePathname.mockReturnValue("/series/foo/chapters/other");

    const { ChapterTocLink } = await import(
      "@shared/components/molecules/link/chapter-toc-link"
    );

    const { unmount } = render(
      <ChapterTocLink
        href="/series/foo/chapters/bar"
        className="inactive"
        activeClassName="active"
      >
        チャプタータイトル
      </ChapterTocLink>,
    );

    const link = screen.getByText("チャプタータイトル");
    expect(link).not.toHaveAttribute("aria-current");

    unmount();
  });

  it("現在のパスと一致する場合は activeClassName が適用される", async () => {
    mockUsePathname.mockReturnValue("/series/foo/chapters/bar");

    const { ChapterTocLink } = await import(
      "@shared/components/molecules/link/chapter-toc-link"
    );

    const { unmount } = render(
      <ChapterTocLink
        href="/series/foo/chapters/bar"
        className="inactiveClass"
        activeClassName="activeClass"
      >
        チャプタータイトル
      </ChapterTocLink>,
    );

    const link = screen.getByText("チャプタータイトル");
    expect(link.className.split(/\s+/)).toContain("activeClass");
    expect(link.className.split(/\s+/)).not.toContain("inactiveClass");

    unmount();
  });

  it("現在のパスと異なる場合は className が適用される", async () => {
    mockUsePathname.mockReturnValue("/series/foo/chapters/other");

    const { ChapterTocLink } = await import(
      "@shared/components/molecules/link/chapter-toc-link"
    );

    const { unmount } = render(
      <ChapterTocLink
        href="/series/foo/chapters/bar"
        className="inactiveClass"
        activeClassName="activeClass"
      >
        チャプタータイトル
      </ChapterTocLink>,
    );

    const link = screen.getByText("チャプタータイトル");
    expect(link.className.split(/\s+/)).toContain("inactiveClass");
    expect(link.className.split(/\s+/)).not.toContain("activeClass");

    unmount();
  });
});
