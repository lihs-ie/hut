/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, act, waitFor } from "@testing-library/react";
import { ContentType } from "@shared/domains/search-token/reference";
import type { SearchReferenceIdentifier } from "@shared/domains/search-token/reference";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleIdentifierMold } from "../../../support/molds/domains/article/common";

describe("components/molecules/view-tracker/ViewTracker", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("マウント時に incrementViewCount が identifier を引数として呼ばれる", async () => {
    const { ViewTracker } = await import(
      "@shared/components/molecules/view-tracker/index"
    );

    const articleIdentifier = Forger(ArticleIdentifierMold).forgeWithSeed(1);
    const identifier: SearchReferenceIdentifier = {
      type: ContentType.ARTICLE,
      content: articleIdentifier,
    };
    const incrementViewCount = vi.fn().mockResolvedValue(undefined);

    await act(async () => {
      render(
        <ViewTracker
          identifier={identifier}
          incrementViewCount={incrementViewCount}
        />,
      );
    });

    await waitFor(() => {
      expect(incrementViewCount).toHaveBeenCalledTimes(1);
      expect(incrementViewCount).toHaveBeenCalledWith(identifier);
    });
  });

  it("incrementViewCount がエラーを投げてもクラッシュしない", async () => {
    const { ViewTracker } = await import(
      "@shared/components/molecules/view-tracker/index"
    );

    const articleIdentifier = Forger(ArticleIdentifierMold).forgeWithSeed(2);
    const identifier: SearchReferenceIdentifier = {
      type: ContentType.ARTICLE,
      content: articleIdentifier,
    };
    const incrementViewCount = vi
      .fn()
      .mockRejectedValue(new Error("Server action error"));

    await act(async () => {
      render(
        <ViewTracker
          identifier={identifier}
          incrementViewCount={incrementViewCount}
        />,
      );
    });

    await waitFor(() => {
      expect(incrementViewCount).toHaveBeenCalledTimes(1);
    });
  });

  it("null を返す（DOM に何も描画しない）", async () => {
    const { ViewTracker } = await import(
      "@shared/components/molecules/view-tracker/index"
    );

    const articleIdentifier = Forger(ArticleIdentifierMold).forgeWithSeed(3);
    const identifier: SearchReferenceIdentifier = {
      type: ContentType.ARTICLE,
      content: articleIdentifier,
    };
    const incrementViewCount = vi.fn().mockResolvedValue(undefined);

    let container: HTMLElement;
    await act(async () => {
      ({ container } = render(
        <ViewTracker
          identifier={identifier}
          incrementViewCount={incrementViewCount}
        />,
      ));
    });

    expect(container!.firstChild).toBeNull();
  });
});
