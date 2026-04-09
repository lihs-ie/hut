/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { render, act, fireEvent } from "@testing-library/react";
import { ToastProvider } from "@shared/components/molecules/toast";
import {
  ChapterMold,
  ChapterSlugMold,
  SeriesMold,
  SeriesSlugMold,
} from "../../../support/molds/domains/series";

const mockShowToast = vi.fn();

vi.mock("next/navigation", () => ({
  useRouter: () => ({
    push: vi.fn(),
  }),
}));

vi.mock("@shared/components/molecules/toast", () => ({
  ToastProvider: (props: { children: React.ReactNode }) => props.children,
  useToast: () => ({ showToast: mockShowToast }),
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

vi.mock("next/link", () => ({
  default: (linkProps: {
    href: string;
    children: React.ReactNode;
    className?: string;
  }) => <a href={linkProps.href} className={linkProps.className}>{linkProps.children}</a>,
}));

vi.mock("@shared/components/atoms/icon/plus", () => ({
  PlusIcon: () => <span>PlusIcon</span>,
}));

vi.mock("@shared/components/atoms/icon/ballpen", () => ({
  BallpenIcon: () => <span>BallpenIcon</span>,
}));

vi.mock("@shared/components/global/hooks/use-image-compression", () => ({
  compressImageToWebP: vi.fn().mockImplementation((file: File) => ({
    _tag: "AsyncResult",
    match: vi.fn().mockImplementation(
      async (handlers: { ok: (value: unknown) => unknown; err: (error: unknown) => unknown }) =>
        handlers.ok({
          blob: file,
          originalSize: file.size,
          compressedSize: file.size,
          width: 100,
          height: 100,
          mimeType: "image/webp",
        }),
    ),
    unwrap: vi.fn(),
  })),
  getCompressedFileName: vi.fn().mockImplementation((name: string) =>
    name.replace(/\.[^.]+$/, ".webp"),
  ),
  CompressedImage: {},
}));

describe("components/organisms/series/SeriesEditOrganism", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("連載新規作成時にpersistが成功するとshowToastが「連載を作成しました」で呼ばれる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { getByText, getByLabelText } = render(
      <ToastProvider>
        <SeriesEditOrganism persist={persist} tags={[]} />
      </ToastProvider>,
    );

    await act(async () => {
      const titleInput = getByLabelText("タイトル");
      fireEvent.change(titleInput, { target: { value: "テスト連載" } });
    });

    await act(async () => {
      getByText("保存").click();
    });

    expect(mockShowToast).toHaveBeenCalledWith("連載を作成しました");
  });

  it("連載編集時にpersistが成功するとshowToastが「連載を更新しました」で呼ばれる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <SeriesEditOrganism
          initial={Forger(SeriesMold).forge({
            title: "既存連載",
            slug: Forger(SeriesSlugMold).forge({ value: "existing-series" }),
            chapters: [],
            tags: [],
          })}
          persist={persist}
          tags={[]}
        />
      </ToastProvider>,
    );

    await act(async () => {
      getByText("保存").click();
    });

    expect(mockShowToast).toHaveBeenCalledWith("連載を更新しました");
  });

  it("タイトル入力フィールドが存在する", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { container } = render(
      <ToastProvider>
        <SeriesEditOrganism persist={persist} tags={[]} />
      </ToastProvider>,
    );

    const titleInput = container.querySelector('input[name="title"]') as HTMLInputElement;
    expect(titleInput).not.toBeNull();
  });

  it("ステータストグルがrole=switchで表示される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { getByRole } = render(
      <ToastProvider>
        <SeriesEditOrganism persist={persist} tags={[]} />
      </ToastProvider>,
    );

    const toggleSwitch = getByRole("switch");
    expect(toggleSwitch).not.toBeNull();
  });

  it("初期状態でDRAFTバッジが表示される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <SeriesEditOrganism persist={persist} tags={[]} />
      </ToastProvider>,
    );

    expect(getByText("下書き")).toBeInTheDocument();
  });

  it("chaptersが空配列で渡された場合、「まだチャプターがありません」が表示される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <SeriesEditOrganism
          initial={Forger(SeriesMold).forge({
            title: "既存連載",
            slug: Forger(SeriesSlugMold).forge({ value: "existing-series" }),
            chapters: [],
            tags: [],
          })}
          persist={persist}
          tags={[]}
          chapters={[]}
          seriesSlug={Forger(SeriesSlugMold).forge({ value: "existing-series" })}
        />
      </ToastProvider>,
    );

    expect(getByText("まだチャプターがありません")).toBeInTheDocument();
  });

  it("chaptersとseriesSlugが両方渡された場合、チャプター管理セクションが表示される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const chapters = [
      Forger(ChapterMold).forge({
        title: "第1章: はじめに",
        slug: Forger(ChapterSlugMold).forge({ value: "chapter-1" }),
        images: [],
      }),
    ];

    const { getByText } = render(
      <ToastProvider>
        <SeriesEditOrganism
          initial={Forger(SeriesMold).forge({
            title: "既存連載",
            slug: Forger(SeriesSlugMold).forge({ value: "existing-series" }),
            chapters: [],
            tags: [],
          })}
          persist={persist}
          tags={[]}
          chapters={chapters}
          seriesSlug={Forger(SeriesSlugMold).forge({ value: "existing-series" })}
        />
      </ToastProvider>,
    );

    expect(getByText("チャプター管理")).toBeInTheDocument();
    expect(getByText("第1章: はじめに")).toBeInTheDocument();
  });

  it("chaptersとseriesSlugが渡されない場合、チャプター管理セクションは表示されない", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { queryByText } = render(
      <ToastProvider>
        <SeriesEditOrganism persist={persist} tags={[]} />
      </ToastProvider>,
    );

    expect(queryByText("チャプター管理")).not.toBeInTheDocument();
  });

  it("uploadImageが渡された場合、カバー画像アップロードボタンが表示される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);
    const uploadImage = vi
      .fn()
      .mockResolvedValue("https://example.com/cover.webp");

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { container } = render(
      <ToastProvider>
        <SeriesEditOrganism
          persist={persist}
          tags={[]}
          uploadImage={uploadImage}
        />
      </ToastProvider>,
    );

    const fileInput = container.querySelector('input[type="file"]');
    expect(fileInput).not.toBeNull();
  });

  it("カバー画像をアップロードするとURLが設定される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);
    const uploadImage = vi
      .fn()
      .mockResolvedValue("https://example.com/cover.webp");

    const { SeriesEditOrganism } = await import(
      "@shared/components/organisms/series/edit"
    );

    const { container } = render(
      <ToastProvider>
        <SeriesEditOrganism
          persist={persist}
          tags={[]}
          uploadImage={uploadImage}
        />
      </ToastProvider>,
    );

    const fileInput = container.querySelector(
      'input[type="file"]',
    ) as HTMLInputElement;
    const file = new File(["cover"], "cover.jpg", { type: "image/jpeg" });

    await act(async () => {
      fireEvent.change(fileInput, { target: { files: [file] } });
    });

    expect(uploadImage).toHaveBeenCalled();
  });
});
