/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesIdentifierMold,
  SeriesSlugMold,
} from "../support/molds/domains/series/common";
import { aggregateNotFoundError } from "@shared/aspects/error";
import {
  setupUnwrapWithResolvedValue,
  setupUnwrapWithRejectedValue,
} from "../support/helpers";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

vi.mock("@shared/providers/workflows/series", () => ({
  SeriesWorkflowProvider: {
    find: vi.fn(),
    findBySlug: vi.fn(),
    search: vi.fn(),
  },
}));

describe("series actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  describe("find", () => {
    it("identifierでシリーズを正常に取得できる", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(series);

      const { find } = await import("@shared/actions/series");
      const result = await find(series.identifier);

      expect(result).toEqual(series);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("シリーズが見つからない場合はエラーがスローされる", async () => {
      const identifier = Forger(SeriesIdentifierMold).forgeWithSeed(1);
      await setupUnwrapWithRejectedValue(
        aggregateNotFoundError("Series", "Series not found.")
      );

      const { find } = await import("@shared/actions/series");

      await expect(find(identifier)).rejects.toThrow();
    });
  });

  describe("findBySlug", () => {
    it("slugでシリーズを正常に取得できる", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(2);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(series);

      const { findBySlug } = await import("@shared/actions/series");
      const result = await findBySlug(series.slug);

      expect(result).toEqual(series);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("slugに対応するシリーズが見つからない場合はエラーがスローされる", async () => {
      const slug = Forger(SeriesSlugMold).forgeWithSeed(1);
      await setupUnwrapWithRejectedValue(
        aggregateNotFoundError("Series", "Series not found.")
      );

      const { findBySlug } = await import("@shared/actions/series");

      await expect(findBySlug(slug)).rejects.toThrow();
    });
  });

  describe("search", () => {
    it("検索条件に一致するシリーズ一覧を取得できる", async () => {
      const seriesList = Forger(SeriesMold).forgeMultiWithSeed(3, 1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(seriesList);

      const { search } = await import("@shared/actions/series");
      const result = await search({ slug: null, tags: null });

      expect(result).toEqual(seriesList);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("titleで検索できる", async () => {
      const seriesList = Forger(SeriesMold).forgeMultiWithSeed(2, 1);
      await setupUnwrapWithResolvedValue(seriesList);

      const { search } = await import("@shared/actions/series");
      const result = await search({ slug: "test", tags: null });

      expect(result).toEqual(seriesList);
    });

    it("検索条件に一致するシリーズがない場合は空配列を返す", async () => {
      await setupUnwrapWithResolvedValue([]);

      const { search } = await import("@shared/actions/series");
      const result = await search({ slug: "non-existent", tags: null });

      expect(result).toEqual([]);
    });
  });
});
