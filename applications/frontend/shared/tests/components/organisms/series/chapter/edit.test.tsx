/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, act } from "@testing-library/react";
import { ToastProvider } from "@shared/components/molecules/toast";

const mockShowToast = vi.fn();

vi.mock("next/navigation", () => ({
  useRouter: () => ({
    push: vi.fn(),
  }),
}));

vi.mock("next/dynamic", () => ({
  default: () => () => <div>MarkdownEditor</div>,
}));

vi.mock("@shared/components/organisms/common/editor/markdown-preview", () => ({
  MarkdownPreview: () => <div>MarkdownPreview</div>,
}));

vi.mock("@shared/components/organisms/common/editor/header", () => ({
  EditorHeader: (props: { persist: () => void; title: string }) => (
    <button type="button" onClick={props.persist}>
      保存
    </button>
  ),
}));

vi.mock("@shared/components/molecules/overlay/loading", () => ({
  LoadingOverlay: () => <div>LoadingOverlay</div>,
}));

vi.mock("@shared/components/molecules/modal/error", () => ({
  ErrorModal: () => <div>ErrorModal</div>,
}));

vi.mock("@shared/components/molecules/toast", () => ({
  ToastProvider: (props: { children: React.ReactNode }) => props.children,
  useToast: () => ({ showToast: mockShowToast }),
}));

vi.mock("@shared/domains/common/markdown", () => ({
  extractImageUrls: () => new Set(),
}));

describe("components/organisms/series/chapter/ChapterEditOrganism", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("チャプター新規作成時にpersistが成功するとshowToastが「チャプターを作成しました」で呼ばれる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { ChapterEditOrganism } = await import(
      "@shared/components/organisms/series/chapter/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <ChapterEditOrganism
          persist={persist}
          uploadImage={vi.fn()}
          seriesSlug="test-series"
        />
      </ToastProvider>,
    );

    await act(async () => {
      getByText("保存").click();
    });

    expect(mockShowToast).toHaveBeenCalledWith("チャプターを作成しました");
  });

  it("チャプター編集時にpersistが成功するとshowToastが「チャプターを更新しました」で呼ばれる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { ChapterEditOrganism } = await import(
      "@shared/components/organisms/series/chapter/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <ChapterEditOrganism
          initial={{
            identifier: "01HWXYZ0000000000000000000" as import("@shared/domains/series/chapter").ChapterIdentifier,
            title: "既存チャプター" as import("@shared/domains/series/chapter").ChapterTitle,
            slug: "existing-chapter" as import("@shared/domains/common").Slug,
            content: "# 既存チャプター\n\nコンテンツ" as import("@shared/domains/series/chapter").Content,
            images: [],
            status: "DRAFT" as import("@shared/domains/common").PublishStatus,
            timeline: { createdAt: new Date(), updatedAt: new Date() },
          } as import("@shared/domains/series/chapter").Chapter}
          persist={persist}
          uploadImage={vi.fn()}
          seriesSlug="test-series"
        />
      </ToastProvider>,
    );

    await act(async () => {
      getByText("保存").click();
    });

    expect(mockShowToast).toHaveBeenCalledWith("チャプターを更新しました");
  });
});
