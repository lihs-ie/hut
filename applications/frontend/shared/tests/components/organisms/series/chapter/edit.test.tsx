/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, act } from "@testing-library/react";
import { Forger } from "@lihs-ie/forger-ts";
import { ToastProvider } from "@shared/components/molecules/toast";
import { ChapterMold } from "../../../../support/molds/domains/series";
import { slugSchema } from "@shared/domains/common/slug";

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
          initial={Forger(ChapterMold).forge({
            title: "既存チャプター",
            content: "# 既存チャプター\n\nコンテンツ",
            images: [],
          })}
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

  it("新規チャプター作成時にpersistにseriesSlugではなく空文字列のslugが渡される", async () => {
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

    expect(persist).toHaveBeenCalledWith(
      expect.objectContaining({
        slug: "",
      }),
    );
  });

  it("新規チャプター作成時の初期コンテンツにCHAPTER_FRONTMATTER_TEMPLATEが使用される", async () => {
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

    expect(persist).toHaveBeenCalledWith(
      expect.objectContaining({
        content: "",
      }),
    );
  });

  it("編集時に既存チャプターのslugがpersistに渡される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { ChapterEditOrganism } = await import(
      "@shared/components/organisms/series/chapter/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <ChapterEditOrganism
          initial={Forger(ChapterMold).forge({
            title: "既存チャプター",
            slug: slugSchema.parse("existing-chapter"),
            content: "# 既存チャプター\n\nコンテンツ",
            images: [],
          })}
          persist={persist}
          uploadImage={vi.fn()}
          seriesSlug="test-series"
        />
      </ToastProvider>,
    );

    await act(async () => {
      getByText("保存").click();
    });

    expect(persist).toHaveBeenCalledWith(
      expect.objectContaining({
        slug: "existing-chapter",
      }),
    );
  });

  it("編集時にpersistに渡されるcontentにfrontmatterが含まれない", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { ChapterEditOrganism } = await import(
      "@shared/components/organisms/series/chapter/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <ChapterEditOrganism
          initial={Forger(ChapterMold).forge({
            title: "既存チャプター",
            content: "本文コンテンツ",
            images: [],
          })}
          persist={persist}
          uploadImage={vi.fn()}
          seriesSlug="test-series"
        />
      </ToastProvider>,
    );

    await act(async () => {
      getByText("保存").click();
    });

    const persistCall = persist.mock.calls[0][0];
    expect(persistCall.content).not.toContain("---");
    expect(persistCall.content).not.toContain("title:");
    expect(persistCall.content).not.toContain("slug:");
  });
});
