/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { isValidElement } from "react";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesSlugMold,
  ChapterMold,
  ChapterIdentifierMold,
} from "../../../../../support/molds/domains/series";

vi.mock("@shared/components/organisms/series/chapter/toc/presenter", () => ({
  ChapterTOCPresenter: () => null,
}));

describe("components/organisms/series/chapter/ChapterTOC", () => {
  it("React 要素を返す", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const identifier = Forger(ChapterIdentifierMold).forge();
    const chapter = Forger(ChapterMold).forge();

    const { ChapterTOC } = await import(
      "@shared/components/organisms/series/chapter/toc"
    );

    const result = await ChapterTOC({
      slug,
      seriesTitle: "テストシリーズ",
      chapters: [identifier],
      findChaptersByIdentifiers: async () => [chapter],
    });

    expect(isValidElement(result)).toBe(true);
  });

  it("findChaptersByIdentifiers に chapters を渡す", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const identifier = Forger(ChapterIdentifierMold).forge();
    const chapter = Forger(ChapterMold).forge();

    const { ChapterTOC } = await import(
      "@shared/components/organisms/series/chapter/toc"
    );

    let capturedIdentifiers: unknown = null;
    await ChapterTOC({
      slug,
      seriesTitle: "テストシリーズ",
      chapters: [identifier],
      findChaptersByIdentifiers: async (identifiers) => {
        capturedIdentifiers = identifiers;
        return [chapter];
      },
    });

    expect(capturedIdentifiers).toEqual([identifier]);
  });
});
