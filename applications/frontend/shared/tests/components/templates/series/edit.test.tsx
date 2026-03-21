/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, act, fireEvent } from "@testing-library/react";
import { ToastProvider } from "@shared/components/molecules/toast";

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

describe("components/templates/series/SeriesEdit", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("連載新規作成時にpersistが成功するとshowToastが「連載を作成しました」で呼ばれる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const { getByText, getByLabelText } = render(
      <ToastProvider>
        <SeriesEdit persist={persist} tags={[]} />
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

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const { getByText } = render(
      <ToastProvider>
        <SeriesEdit
          initial={{
            identifier: "01HWXYZ0000000000000000000",
            title: "既存連載",
            slug: "existing-series" as import("@shared/domains/common").Slug,
            subTitle: null,
            description: undefined,
            cover: null,
            tags: [],
            chapters: [],
            status: "DRAFT" as import("@shared/domains/common").PublishStatus,
            timeline: { createdAt: new Date(), updatedAt: new Date() },
          } as import("@shared/domains/series").Series}
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

  it("タイトルが入力できる", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const { container } = render(
      <ToastProvider>
        <SeriesEdit persist={persist} tags={[]} />
      </ToastProvider>,
    );

    const titleInput = container.querySelector('input[name="title"]') as HTMLInputElement;
    expect(titleInput).not.toBeNull();
  });

  it("ステータストグルが表示される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const { getByRole } = render(
      <ToastProvider>
        <SeriesEdit persist={persist} tags={[]} />
      </ToastProvider>,
    );

    const checkbox = getByRole("checkbox");
    expect(checkbox).not.toBeNull();
  });

  it("chaptersとseriesSlugが両方渡された場合、チャプター管理セクションが表示される", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const chapters = [
      {
        identifier: "01HWXYZ0000000000000000001",
        title: "第1章: はじめに",
        slug: "chapter-1" as import("@shared/domains/common").Slug,
        content: "",
        images: [],
        status: "PUBLISHED" as import("@shared/domains/common").PublishStatus,
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      } as import("@shared/domains/series/chapter").Chapter,
    ];

    const { getByText } = render(
      <ToastProvider>
        <SeriesEdit
          initial={{
            identifier: "01HWXYZ0000000000000000000",
            title: "既存連載",
            slug: "existing-series" as import("@shared/domains/common").Slug,
            subTitle: null,
            description: undefined,
            cover: null,
            tags: [],
            chapters: [],
            status: "DRAFT" as import("@shared/domains/common").PublishStatus,
            timeline: { createdAt: new Date(), updatedAt: new Date() },
          } as import("@shared/domains/series").Series}
          persist={persist}
          tags={[]}
          chapters={chapters}
          seriesSlug={"existing-series" as import("@shared/domains/common").Slug}
        />
      </ToastProvider>,
    );

    expect(getByText("チャプター管理")).toBeInTheDocument();
    expect(getByText("第1章: はじめに")).toBeInTheDocument();
  });

  it("chaptersとseriesSlugが渡されない場合、チャプター管理セクションは表示されない", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const { queryByText } = render(
      <ToastProvider>
        <SeriesEdit persist={persist} tags={[]} />
      </ToastProvider>,
    );

    expect(queryByText("チャプター管理")).not.toBeInTheDocument();
  });

  it("chaptersのみ渡されseriesSlugがない場合、チャプター管理セクションは表示されない", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const chapters = [
      {
        identifier: "01HWXYZ0000000000000000001",
        title: "第1章",
        slug: "chapter-1" as import("@shared/domains/common").Slug,
        content: "",
        images: [],
        status: "PUBLISHED" as import("@shared/domains/common").PublishStatus,
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      } as import("@shared/domains/series/chapter").Chapter,
    ];

    const { queryByText } = render(
      <ToastProvider>
        <SeriesEdit persist={persist} tags={[]} chapters={chapters} />
      </ToastProvider>,
    );

    expect(queryByText("チャプター管理")).not.toBeInTheDocument();
  });
});
