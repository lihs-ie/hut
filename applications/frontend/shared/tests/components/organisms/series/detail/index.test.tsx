/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesSlugMold,
  ChapterMold,
} from "../../../../support/molds/domains/series";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";

vi.mock("next/link", () => ({
  default: (linkProps: {
    href: string;
    children: React.ReactNode;
    className?: string;
  }) => <a href={linkProps.href} className={linkProps.className}>{linkProps.children}</a>,
}));

vi.mock("@shared/components/atoms/icon/chevron-right", () => ({
  ChevronRightIcon: () => null,
}));

vi.mock("@shared/components/organisms/common/list/tag.presenter", () => ({
  TagBadgeListPresenter: () => null,
}));

describe("components/organisms/series/detail/SeriesDetailPresenter", () => {
  it("React 要素を返す", async () => {
    const chapters = [Forger(ChapterMold).forge()];
    const series = Forger(SeriesMold).forge({ chapters: [] });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetailPresenter } = await import(
      "@shared/components/organisms/series/detail/index.presenter"
    );

    const { unmount } = render(
      SeriesDetailPresenter({
        series,
        slug,
        chapters,
        tags: [],
      })
    );

    expect(screen.getByRole("heading", { level: 1 })).toHaveTextContent(
      series.title
    );

    unmount();
  });

  it("シリーズタイトルが表示される", async () => {
    const chapters = [Forger(ChapterMold).forge()];
    const series = Forger(SeriesMold).forge({ chapters: [] });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetailPresenter } = await import(
      "@shared/components/organisms/series/detail/index.presenter"
    );

    const { unmount } = render(
      SeriesDetailPresenter({
        series,
        slug,
        chapters,
        tags: [],
      })
    );

    expect(screen.getByRole("heading", { level: 1 })).toHaveTextContent(
      series.title
    );

    unmount();
  });

  it("チャプターリストが2桁ゼロ埋め番号で表示される", async () => {
    const chapters = [
      Forger(ChapterMold).forge({ title: "第1章" }),
      Forger(ChapterMold).forgeWithSeed(2, { title: "第2章" }),
    ];
    const series = Forger(SeriesMold).forge({ chapters: [] });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetailPresenter } = await import(
      "@shared/components/organisms/series/detail/index.presenter"
    );

    const { unmount } = render(
      SeriesDetailPresenter({
        series,
        slug,
        chapters,
        tags: [],
      })
    );

    expect(screen.getByText("01")).toBeInTheDocument();
    expect(screen.getByText("02")).toBeInTheDocument();

    unmount();
  });

  it("9章を超えるチャプター番号も2桁でゼロ埋めされる", async () => {
    const chapters = Array.from({ length: 10 }, (_, i) =>
      Forger(ChapterMold).forgeWithSeed(i + 1, { title: `第${i + 1}章` })
    );
    const series = Forger(SeriesMold).forge({ chapters: [] });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetailPresenter } = await import(
      "@shared/components/organisms/series/detail/index.presenter"
    );

    const { unmount } = render(
      SeriesDetailPresenter({
        series,
        slug,
        chapters,
        tags: [],
      })
    );

    expect(screen.getByText("10")).toBeInTheDocument();

    unmount();
  });

  it("サブタイトルがある場合は表示される", async () => {
    const series = Forger(SeriesMold).forge({
      subTitle: "サブタイトルテスト" as import("@shared/domains/series").SubTitle,
      chapters: [],
    });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetailPresenter } = await import(
      "@shared/components/organisms/series/detail/index.presenter"
    );

    const { unmount } = render(
      SeriesDetailPresenter({
        series,
        slug,
        chapters: [],
        tags: [],
      })
    );

    expect(screen.getByText("サブタイトルテスト")).toBeInTheDocument();

    unmount();
  });

  it("編集リンクが表示されない", async () => {
    const chapters = [Forger(ChapterMold).forge()];
    const series = Forger(SeriesMold).forge({ chapters: [] });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetailPresenter } = await import(
      "@shared/components/organisms/series/detail/index.presenter"
    );

    const { unmount } = render(
      SeriesDetailPresenter({
        series,
        slug,
        chapters,
        tags: [],
      })
    );

    expect(screen.queryByText("編集")).not.toBeInTheDocument();
    expect(screen.queryByText("チャプターを追加")).not.toBeInTheDocument();

    unmount();
  });

  it("複数の章があっても React 要素を返す", async () => {
    const chapters = [
      Forger(ChapterMold).forge({ title: "第1章" }),
      Forger(ChapterMold).forgeWithSeed(2, { title: "第2章" }),
    ];
    const series = Forger(SeriesMold).forge({ chapters: [] });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetailPresenter } = await import(
      "@shared/components/organisms/series/detail/index.presenter"
    );

    const { unmount } = render(
      SeriesDetailPresenter({
        series,
        slug,
        chapters,
        tags: [],
      })
    );

    expect(screen.getByText("第1章")).toBeInTheDocument();
    expect(screen.getByText("第2章")).toBeInTheDocument();

    unmount();
  });

  it("タグを受け取った場合も正常にレンダリングされる", async () => {
    const tags = Forger(TagMold).forgeMultiWithSeed(2, 1);
    const series = Forger(SeriesMold).forge({ chapters: [] });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetailPresenter } = await import(
      "@shared/components/organisms/series/detail/index.presenter"
    );

    const { unmount } = render(
      SeriesDetailPresenter({
        series,
        slug,
        chapters: [],
        tags,
      })
    );

    expect(screen.getByRole("heading", { level: 1 })).toBeInTheDocument();

    unmount();
  });
});
