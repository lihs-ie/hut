import { fireEvent, render, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";

import { TOCDrawer } from "@shared/components/organisms/series/chapter/toc/drawer";

const mockMatchMedia = (matches: boolean) => {
  Object.defineProperty(window, "matchMedia", {
    writable: true,
    configurable: true,
    value: vi.fn().mockImplementation((query: string) => ({
      matches,
      media: query,
      onchange: null,
      addEventListener: vi.fn(),
      removeEventListener: vi.fn(),
      addListener: vi.fn(),
      removeListener: vi.fn(),
      dispatchEvent: vi.fn(),
    })),
  });
};

describe("TOCDrawer", () => {
  beforeEach(() => {
    mockMatchMedia(false);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  test("初期状態では drawer は閉じている", () => {
    render(
      <TOCDrawer>
        <div>目次コンテンツ</div>
      </TOCDrawer>,
    );

    const drawer = screen.getByRole("button", { name: "目次を開く" });
    expect(drawer).toHaveAttribute("aria-expanded", "false");
  });

  test("trigger ボタンをクリックすると drawer が開く", () => {
    render(
      <TOCDrawer>
        <div>目次コンテンツ</div>
      </TOCDrawer>,
    );

    fireEvent.click(screen.getByRole("button", { name: "目次を開く" }));

    expect(screen.getByRole("button", { name: "目次を開く" })).toHaveAttribute(
      "aria-expanded",
      "true",
    );
    expect(
      screen.getByRole("button", { name: "目次を閉じる" }),
    ).toBeInTheDocument();
  });

  test("overlay クリックで drawer が閉じる", () => {
    const { container } = render(
      <TOCDrawer>
        <div>目次コンテンツ</div>
      </TOCDrawer>,
    );

    fireEvent.click(screen.getByRole("button", { name: "目次を開く" }));

    const overlay = container.querySelector('div[aria-hidden="true"]');
    expect(overlay).not.toBeNull();
    fireEvent.click(overlay!);

    expect(screen.getByRole("button", { name: "目次を開く" })).toHaveAttribute(
      "aria-expanded",
      "false",
    );
  });

  test("close ボタンクリックで drawer が閉じる", () => {
    render(
      <TOCDrawer>
        <div>目次コンテンツ</div>
      </TOCDrawer>,
    );

    fireEvent.click(screen.getByRole("button", { name: "目次を開く" }));
    fireEvent.click(screen.getByRole("button", { name: "目次を閉じる" }));

    expect(screen.getByRole("button", { name: "目次を開く" })).toHaveAttribute(
      "aria-expanded",
      "false",
    );
  });

  test("Escape キーで drawer が閉じる", () => {
    render(
      <TOCDrawer>
        <div>目次コンテンツ</div>
      </TOCDrawer>,
    );

    fireEvent.click(screen.getByRole("button", { name: "目次を開く" }));
    fireEvent.keyDown(document, { key: "Escape" });

    expect(screen.getByRole("button", { name: "目次を開く" })).toHaveAttribute(
      "aria-expanded",
      "false",
    );
  });

  test("drawer 内の link クリックで drawer が閉じる", () => {
    render(
      <TOCDrawer>
        <a href="/series/foo/chapters/next">次のチャプター</a>
      </TOCDrawer>,
    );

    fireEvent.click(screen.getByRole("button", { name: "目次を開く" }));
    fireEvent.click(screen.getByRole("link", { name: "次のチャプター" }));

    expect(screen.getByRole("button", { name: "目次を開く" })).toHaveAttribute(
      "aria-expanded",
      "false",
    );
  });

  test("children が drawer 内に描画される", () => {
    render(
      <TOCDrawer>
        <div data-testid="toc-content">目次コンテンツ</div>
      </TOCDrawer>,
    );

    expect(screen.getByTestId("toc-content")).toBeInTheDocument();
  });

  test("mobile で drawer が閉じているとき inert と aria-hidden が付く", () => {
    mockMatchMedia(true);

    const { container } = render(
      <TOCDrawer>
        <div>目次コンテンツ</div>
      </TOCDrawer>,
    );

    const drawer = container.querySelector("#chapter-toc-drawer");
    expect(drawer).toHaveAttribute("aria-hidden", "true");
    expect(drawer).toHaveAttribute("inert");
  });

  test("mobile で drawer が開いているとき inert と aria-hidden が外れる", () => {
    mockMatchMedia(true);

    const { container } = render(
      <TOCDrawer>
        <div>目次コンテンツ</div>
      </TOCDrawer>,
    );

    fireEvent.click(screen.getByRole("button", { name: "目次を開く" }));

    const drawer = container.querySelector("#chapter-toc-drawer");
    expect(drawer).toHaveAttribute("aria-hidden", "false");
    expect(drawer).not.toHaveAttribute("inert");
  });

  test("desktop では drawer が閉じていても inert/aria-hidden が付かない", () => {
    mockMatchMedia(false);

    const { container } = render(
      <TOCDrawer>
        <div>目次コンテンツ</div>
      </TOCDrawer>,
    );

    const drawer = container.querySelector("#chapter-toc-drawer");
    expect(drawer).toHaveAttribute("aria-hidden", "false");
    expect(drawer).not.toHaveAttribute("inert");
  });
});
