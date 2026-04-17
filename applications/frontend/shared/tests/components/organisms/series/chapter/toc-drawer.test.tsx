import { fireEvent, render, screen } from "@testing-library/react";
import { describe, expect, test } from "vitest";

import { TocDrawer } from "@shared/components/organisms/series/chapter/toc-drawer";

describe("TocDrawer", () => {
  test("初期状態では drawer は閉じている", () => {
    render(
      <TocDrawer>
        <div>目次コンテンツ</div>
      </TocDrawer>,
    );

    const drawer = screen.getByRole("button", { name: "目次を開く" });
    expect(drawer).toHaveAttribute("aria-expanded", "false");
  });

  test("trigger ボタンをクリックすると drawer が開く", () => {
    render(
      <TocDrawer>
        <div>目次コンテンツ</div>
      </TocDrawer>,
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
      <TocDrawer>
        <div>目次コンテンツ</div>
      </TocDrawer>,
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
      <TocDrawer>
        <div>目次コンテンツ</div>
      </TocDrawer>,
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
      <TocDrawer>
        <div>目次コンテンツ</div>
      </TocDrawer>,
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
      <TocDrawer>
        <a href="/series/foo/chapters/next">次のチャプター</a>
      </TocDrawer>,
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
      <TocDrawer>
        <div data-testid="toc-content">目次コンテンツ</div>
      </TocDrawer>,
    );

    expect(screen.getByTestId("toc-content")).toBeInTheDocument();
  });
});
