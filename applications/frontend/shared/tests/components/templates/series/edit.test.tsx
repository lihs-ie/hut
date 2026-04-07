/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { render } from "@testing-library/react";
import { SeriesMold } from "../../../support/molds/domains/series";

vi.mock("@shared/components/organisms/series/edit", () => ({
  SeriesEditOrganism: (props: {
    persist: () => Promise<void>;
    tags: unknown[];
    initial?: unknown;
    chapters?: unknown[];
    seriesSlug?: string;
  }) => (
    <div data-testid="series-edit-organism">
      <span data-testid="has-initial">{props.initial ? "true" : "false"}</span>
      <span data-testid="tags-count">{props.tags.length}</span>
      {props.chapters !== undefined && (
        <span data-testid="has-chapters">true</span>
      )}
      {props.seriesSlug !== undefined && (
        <span data-testid="has-series-slug">true</span>
      )}
    </div>
  ),
}));

describe("components/templates/series/SeriesEdit", () => {
  it("SeriesEditOrganismをレンダリングする", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const { getByTestId } = render(
      <SeriesEdit persist={persist} tags={[]} />,
    );

    expect(getByTestId("series-edit-organism")).toBeInTheDocument();
  });

  it("initialをSeriesEditOrganismに渡す", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const { getByTestId } = render(
      <SeriesEdit
        initial={Forger(SeriesMold).forge({ chapters: [], tags: [] })}
        persist={persist}
        tags={[]}
      />,
    );

    expect(getByTestId("has-initial").textContent).toBe("true");
  });

  it("chaptersとseriesSlugをSeriesEditOrganismに渡す", async () => {
    const persist = vi.fn().mockResolvedValue(undefined);

    const { SeriesEdit } = await import(
      "@shared/components/templates/series/edit"
    );

    const { getByTestId } = render(
      <SeriesEdit
        persist={persist}
        tags={[]}
        chapters={[]}
        seriesSlug={"existing-series" as import("@shared/domains/common").Slug}
      />,
    );

    expect(getByTestId("has-chapters")).toBeInTheDocument();
    expect(getByTestId("has-series-slug")).toBeInTheDocument();
  });
});
