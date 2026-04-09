/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { SeriesChapters } from "@shared/components/organisms/series/detail/chapters";
import { seriesIdentifierSchema } from "@shared/domains/series/common";

vi.mock(
  "@shared/components/molecules/list/card/series/chapter-title",
  () => ({
    ChapterTitleCard: (props: { title: string }) => (
      <div data-testid="chapter-card">{props.title}</div>
    ),
  })
);

const seriesIdentifier = seriesIdentifierSchema.parse("01ARZ3NDEKTSV4RRFFQ69G5FAV");

describe("components/organisms/series/detail/SeriesChapters", () => {
  describe("空状態表示", () => {
    it("チャプターが0件の場合、空状態メッセージが表示される", () => {
      render(
        <SeriesChapters
          chapters={[]}
          slug="test-series"
          series={seriesIdentifier}
        />
      );

      expect(
        screen.getByText("チャプターがありません")
      ).toBeInTheDocument();
      expect(
        screen.getByText("チャプターが追加されるとここに表示されます")
      ).toBeInTheDocument();
    });

    it("チャプターが0件の場合、カードは表示されない", () => {
      render(
        <SeriesChapters
          chapters={[]}
          slug="test-series"
          series={seriesIdentifier}
        />
      );

      expect(screen.queryByTestId("chapter-card")).not.toBeInTheDocument();
    });
  });

  describe("目次見出し", () => {
    it("チャプターが0件でも目次見出しは表示される", () => {
      render(
        <SeriesChapters
          chapters={[]}
          slug="test-series"
          series={seriesIdentifier}
        />
      );

      expect(screen.getByText("目次")).toBeInTheDocument();
    });
  });
});
