/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { render, act } from "@testing-library/react";
import { ToastProvider } from "@shared/components/molecules/toast";
import {
  ArticleMold,
  ContentMold,
  ExcerptMold,
  SlugMold,
  TitleMold,
} from "../../../support/molds/domains/article";

const mockRouterPush = vi.fn();
const mockShowToast = vi.fn();

vi.mock("next/navigation", () => ({
  useRouter: () => ({
    push: mockRouterPush,
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

vi.mock("@shared/components/molecules/select/tag", () => ({
  TagSelect: () => <div>TagSelect</div>,
}));

vi.mock("@shared/components/molecules/overlay/loading", () => ({
  LoadingOverlay: () => <div>LoadingOverlay</div>,
}));

vi.mock("@shared/components/molecules/modal/error", () => ({
  ErrorModal: () => <div>ErrorModal</div>,
}));

vi.mock("@shared/components/atoms/icon/tag", () => ({
  TagIcon: () => <span>TagIcon</span>,
}));

vi.mock("@shared/components/atoms/icon/cross", () => ({
  CrossIcon: () => <span>CrossIcon</span>,
}));

vi.mock("@shared/components/molecules/toast", () => ({
  ToastProvider: (props: { children: React.ReactNode }) => props.children,
  useToast: () => ({ showToast: mockShowToast }),
}));

vi.mock("@shared/components/global/matter", () => ({
  articleMatter: vi.fn().mockReturnValue({
    toAsync: () => ({
      andThen: (_callback: unknown) => ({
        tap: (_tapCallback: unknown) => ({
          match: (handlers: { ok: () => void; err: (error: Error) => void }) =>
            handlers.ok(),
        }),
      }),
    }),
  }),
  ARTICLE_FRONTMATTER_TEMPLATE: "---\ntitle: \n---\n",
  updateFrontmatterTitle: (content: string) => content,
  updateFrontmatterTags: (content: string) => content,
  extractFrontmatterTitle: () => null,
  extractFrontmatterSlug: () => null,
  extractFrontmatterTags: () => null,
}));

vi.mock("@shared/domains/common/markdown", () => ({
  extractImageUrls: () => new Set(),
}));

describe("components/templates/article/ArticleEdit", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("記事作成時にpersistが成功するとshowToastが「記事を作成しました」で呼ばれる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { ArticleEdit } = await import(
      "@shared/components/templates/article/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <ArticleEdit
          persist={persist}
          tags={[]}
          uploadImage={vi.fn()}
        />
      </ToastProvider>,
    );

    await act(async () => {
      getByText("保存").click();
    });

    expect(mockShowToast).toHaveBeenCalledWith("記事を作成しました");
  });

  it("記事編集時にpersistが成功するとshowToastが「記事を更新しました」で呼ばれる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { ArticleEdit } = await import(
      "@shared/components/templates/article/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <ArticleEdit
          initial={Forger(ArticleMold).forge({
            title: Forger(TitleMold).forge({ value: "既存記事" }),
            slug: Forger(SlugMold).forge({ value: "existing-article" }),
            content: Forger(ContentMold).forge({
              value: "---\ntitle: 既存記事\n---\n",
            }),
            excerpt: Forger(ExcerptMold).forge({ value: "既存の抜粋" }),
            images: [],
            tags: [],
          })}
          persist={persist}
          tags={[]}
          uploadImage={vi.fn()}
        />
      </ToastProvider>,
    );

    await act(async () => {
      getByText("保存").click();
    });

    expect(mockShowToast).toHaveBeenCalledWith("記事を更新しました");
  });
});
