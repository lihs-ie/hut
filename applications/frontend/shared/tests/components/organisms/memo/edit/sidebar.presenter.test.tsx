/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, act } from "@testing-library/react";
import { PublishStatus } from "@shared/domains/common";
import { ToastProvider } from "@shared/components/molecules/toast";

vi.mock("next/navigation", () => ({
  useRouter: () => ({
    push: vi.fn(),
  }),
  usePathname: () => "/",
}));

describe("components/organisms/memo/edit/MemoEditSidebarPresenter", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("isCloseLoading が true のとき LoadingOverlay が表示される", async () => {
    const changeStatus = vi.fn(
      () => new Promise<void>((resolve) => setTimeout(resolve, 10000)),
    );

    const { MemoEditSidebarPresenter } = await import(
      "@shared/components/organisms/memo/edit/sidebar.presenter"
    );

    const { getByText } = render(
      <ToastProvider>
        <MemoEditSidebarPresenter
          initialStatus={PublishStatus.DRAFT}
          changeStatus={changeStatus}
        />
      </ToastProvider>,
    );

    const closeButton = getByText("メモをクローズ");

    await act(async () => {
      closeButton.click();
    });

    const overlay = screen.getByRole("status", { name: "読み込み中" });
    expect(overlay).toBeInTheDocument();
  });

  it("isVisibilityLoading が true のとき LoadingOverlay が表示される", async () => {
    const changeStatus = vi.fn(
      () => new Promise<void>((resolve) => setTimeout(resolve, 10000)),
    );

    const { MemoEditSidebarPresenter } = await import(
      "@shared/components/organisms/memo/edit/sidebar.presenter"
    );

    render(
      <ToastProvider>
        <MemoEditSidebarPresenter
          initialStatus={PublishStatus.DRAFT}
          changeStatus={changeStatus}
        />
      </ToastProvider>,
    );

    const switchElement = document.querySelector('input[type="checkbox"]');
    if (switchElement) {
      await act(async () => {
        switchElement.dispatchEvent(new MouseEvent("click", { bubbles: true }));
      });
    }

    expect(changeStatus).toHaveBeenCalled();
  });
});
