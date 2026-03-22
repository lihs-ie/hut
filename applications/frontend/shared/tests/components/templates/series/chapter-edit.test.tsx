/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render } from "@testing-library/react";

vi.mock("@shared/components/organisms/series/chapter/edit", () => ({
  ChapterEditOrganism: (props: {
    seriesSlug: string;
    initial?: unknown;
  }) => (
    <div data-testid="chapter-edit-organism" data-series-slug={props.seriesSlug}>
      ChapterEditOrganism
    </div>
  ),
}));

describe("components/templates/series/chapter/ChapterEdit", () => {
  it("ChapterEditOrganismをレンダリングする", async () => {
    const { ChapterEdit } = await import(
      "@shared/components/templates/series/chapter/edit"
    );

    const { getByTestId } = render(
      <ChapterEdit
        persist={vi.fn()}
        uploadImage={vi.fn()}
        seriesSlug="test-series"
      />,
    );

    expect(getByTestId("chapter-edit-organism")).toBeDefined();
  });

  it("seriesSlugをChapterEditOrganismに渡す", async () => {
    const { ChapterEdit } = await import(
      "@shared/components/templates/series/chapter/edit"
    );

    const { getByTestId } = render(
      <ChapterEdit
        persist={vi.fn()}
        uploadImage={vi.fn()}
        seriesSlug="my-series"
      />,
    );

    expect(getByTestId("chapter-edit-organism").getAttribute("data-series-slug")).toBe("my-series");
  });
});
