/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import React from "react";

vi.mock("@shared/components/templates/search", () => ({
  SearchIndex: vi.fn(() => null),
}));

vi.mock("@shared/actions/tag", () => ({
  getAllTags: vi.fn(),
  findAllTags: vi.fn(),
  ofNames: vi.fn(),
}));

vi.mock("@shared/actions/search-token", () => ({
  searchByToken: vi.fn(),
}));

import SearchPage from "../../../src/pages/search/page";

describe("SearchPage", () => {
  it("freeWord が 100 文字を超える場合は 100 文字に切り詰められる", async () => {
    const longFreeWord = "a".repeat(150);
    const searchParams = Promise.resolve({ freeWord: longFreeWord });

    const element = await SearchPage({ searchParams });
    const props = (element as React.ReactElement).props as {
      unvalidatedCriteria: { freeWord: string | null };
    };

    expect(props.unvalidatedCriteria.freeWord).toHaveLength(100);
    expect(props.unvalidatedCriteria.freeWord).toBe("a".repeat(100));
  });

  it("freeWord が 100 文字以内の場合はそのまま渡される", async () => {
    const freeWord = "短い検索ワード";
    const searchParams = Promise.resolve({ freeWord });

    const element = await SearchPage({ searchParams });
    const props = (element as React.ReactElement).props as {
      unvalidatedCriteria: { freeWord: string | null };
    };

    expect(props.unvalidatedCriteria.freeWord).toBe(freeWord);
  });

  it("freeWord が未指定の場合は null が渡される", async () => {
    const searchParams = Promise.resolve({});

    const element = await SearchPage({ searchParams });
    const props = (element as React.ReactElement).props as {
      unvalidatedCriteria: { freeWord: string | null };
    };

    expect(props.unvalidatedCriteria.freeWord).toBeNull();
  });
});
