/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

const mockShowToast = vi.fn();
const mockUseServerAction = vi.fn();

vi.mock("@shared/components/molecules/toast", () => ({
  useToast: () => ({ showToast: mockShowToast }),
}));

vi.mock("@shared/components/global/hooks/use-server-action", () => ({
  useServerAction: mockUseServerAction,
}));

vi.mock("@shared/components/molecules/overlay/loading", () => ({
  LoadingOverlay: () => null,
}));

vi.mock("@shared/components/molecules/modal/confirm", () => ({
  ConfirmModal: () => null,
}));

vi.mock("@shared/components/molecules/modal/error", () => ({
  ErrorModal: () => null,
}));

vi.mock(
  "@/app/admin/_components/molecules/list/card/content",
  () => ({
    AdminContentCard: () => null,
  }),
);

describe("components/molecules/list/AdminContentListPresenter", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockUseServerAction.mockReturnValue({
      execute: vi.fn(),
      error: null,
      reset: vi.fn(),
      isLoading: false,
    });
  });

  it("useServerActionにonSuccessが渡され、コンテンツを削除しましたメッセージが設定されている", async () => {
    const { renderHook } = await import("@testing-library/react");
    const { AdminContentListPresenter } = await import(
      "@/app/admin/_components/molecules/list/content.presenter"
    );

    renderHook(() => AdminContentListPresenter({ contents: [] }));

    expect(mockUseServerAction).toHaveBeenCalledWith(
      expect.any(Function),
      expect.objectContaining({
        onSuccess: expect.any(Function),
      }),
    );

    const options = mockUseServerAction.mock.calls[0][1];
    options.onSuccess();

    expect(mockShowToast).toHaveBeenCalledWith("コンテンツを削除しました");
  });
});
