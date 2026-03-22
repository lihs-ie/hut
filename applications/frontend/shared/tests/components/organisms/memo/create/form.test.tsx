/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, act, fireEvent } from "@testing-library/react";
import { ToastProvider } from "@shared/components/molecules/toast";

vi.mock("next/navigation", () => ({
  useRouter: () => ({
    push: vi.fn(),
  }),
}));

describe("components/organisms/memo/create/MemoCreateForm", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("slug 入力フィールドが表示される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { MemoCreateForm } = await import(
      "@shared/components/organisms/memo/create/form"
    );

    render(
      <ToastProvider>
        <MemoCreateForm persist={persist} />
      </ToastProvider>,
    );

    const slugInput = screen.getByPlaceholderText("Enter slug...");
    expect(slugInput).toBeInTheDocument();
  });

  it("slug が空のとき送信ボタンが無効になる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { MemoCreateForm } = await import(
      "@shared/components/organisms/memo/create/form"
    );

    render(
      <ToastProvider>
        <MemoCreateForm persist={persist} />
      </ToastProvider>,
    );

    const submitButton = screen.getByRole("button", { name: "メモを作成" });
    expect(submitButton).toBeDisabled();
  });

  it("title と slug が入力されているとき送信ボタンが有効になる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { MemoCreateForm } = await import(
      "@shared/components/organisms/memo/create/form"
    );

    render(
      <ToastProvider>
        <MemoCreateForm persist={persist} />
      </ToastProvider>,
    );

    const titleTextarea = screen.getByPlaceholderText("Enter title...");
    const slugInput = screen.getByPlaceholderText("Enter slug...");

    await act(async () => {
      fireEvent.change(titleTextarea, { target: { value: "Test Title" } });
      fireEvent.change(slugInput, { target: { value: "test-slug" } });
    });

    const submitButton = screen.getByRole("button", { name: "メモを作成" });
    expect(submitButton).not.toBeDisabled();
  });

  it("slug に不正な文字が含まれているとき送信ボタンが無効になる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { MemoCreateForm } = await import(
      "@shared/components/organisms/memo/create/form"
    );

    render(
      <ToastProvider>
        <MemoCreateForm persist={persist} />
      </ToastProvider>,
    );

    const titleTextarea = screen.getByPlaceholderText("Enter title...");
    const slugInput = screen.getByPlaceholderText("Enter slug...");

    await act(async () => {
      fireEvent.change(titleTextarea, { target: { value: "Test Title" } });
      fireEvent.change(slugInput, { target: { value: "Invalid Slug!" } });
    });

    const submitButton = screen.getByRole("button", { name: "メモを作成" });
    expect(submitButton).toBeDisabled();
  });

  it("フォーム送信時に入力した slug が persist に渡される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { MemoCreateForm } = await import(
      "@shared/components/organisms/memo/create/form"
    );

    render(
      <ToastProvider>
        <MemoCreateForm persist={persist} />
      </ToastProvider>,
    );

    const titleTextarea = screen.getByPlaceholderText("Enter title...");
    const slugInput = screen.getByPlaceholderText("Enter slug...");

    await act(async () => {
      fireEvent.change(titleTextarea, { target: { value: "Test Title" } });
      fireEvent.change(slugInput, { target: { value: "my-slug" } });
    });

    const submitButton = screen.getByRole("button", { name: "メモを作成" });

    await act(async () => {
      fireEvent.click(submitButton);
    });

    expect(persist).toHaveBeenCalledWith(
      expect.objectContaining({ slug: "my-slug" }),
    );
  });
});
