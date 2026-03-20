/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";

const mockStartNavigation = vi.fn();

vi.mock("@shared/components/molecules/navigation/provider", () => ({
  useNavigation: () => ({
    isNavigating: false,
    startNavigation: mockStartNavigation,
  }),
}));

vi.mock("next/link", () => ({
  default: ({
    href,
    children,
    onClick,
    ...rest
  }: {
    href: string;
    children: React.ReactNode;
    onClick?: React.MouseEventHandler<HTMLAnchorElement>;
  }) => (
    <a href={href} onClick={onClick} {...rest}>
      {children}
    </a>
  ),
}));

describe("components/molecules/link/NavigableLink", () => {
  it("通常のリンクがレンダリングされる", async () => {
    mockStartNavigation.mockClear();
    const { NavigableLink } = await import(
      "@shared/components/molecules/link/navigable"
    );

    render(<NavigableLink href="/articles">記事一覧</NavigableLink>);

    expect(screen.getByText("記事一覧")).toBeInTheDocument();
  });

  it("クリック時に startNavigation が呼ばれる", async () => {
    mockStartNavigation.mockClear();
    const { NavigableLink } = await import(
      "@shared/components/molecules/link/navigable"
    );

    render(<NavigableLink href="/articles">記事一覧</NavigableLink>);

    fireEvent.click(screen.getByText("記事一覧"));

    expect(mockStartNavigation).toHaveBeenCalledTimes(1);
  });

  it("アンカーリンク (#hash) クリック時は startNavigation が呼ばれない", async () => {
    mockStartNavigation.mockClear();
    const { NavigableLink } = await import(
      "@shared/components/molecules/link/navigable"
    );

    render(
      <NavigableLink href="#section">セクションへ</NavigableLink>,
    );

    fireEvent.click(screen.getByText("セクションへ"));

    expect(mockStartNavigation).not.toHaveBeenCalled();
  });

  it("target=_blank のリンクをクリック時は startNavigation が呼ばれない", async () => {
    mockStartNavigation.mockClear();
    const { NavigableLink } = await import(
      "@shared/components/molecules/link/navigable"
    );

    render(
      <NavigableLink href="/articles" target="_blank">
        外部リンク
      </NavigableLink>,
    );

    fireEvent.click(screen.getByText("外部リンク"));

    expect(mockStartNavigation).not.toHaveBeenCalled();
  });

  it("className が渡されたとき適用される", async () => {
    mockStartNavigation.mockClear();
    const { NavigableLink } = await import(
      "@shared/components/molecules/link/navigable"
    );

    render(
      <NavigableLink href="/articles" className="custom">
        記事一覧
      </NavigableLink>,
    );

    const link = screen.getByText("記事一覧");
    expect(link).toHaveClass("custom");
  });
});
