import { describe, it, expect } from "vitest";
import { recordSearchLog } from "@shared/actions/search-log";
import { UnvalidatedCriteria } from "@shared/domains/search-token/common";

const buildCriteria = (
  overrides: Partial<UnvalidatedCriteria> = {},
): UnvalidatedCriteria => ({
  freeWord: null,
  tags: null,
  type: null,
  sortBy: null,
  order: null,
  limit: null,
  ...overrides,
});

describe("Feature: Search Log Action", () => {
  describe("recordSearchLog", () => {
    it("freeWord が空文字の場合は何も実行しない", async () => {
      await expect(
        recordSearchLog(buildCriteria({ freeWord: "" }), 0),
      ).resolves.toBeUndefined();
    });

    it("freeWord が null の場合は何も実行しない", async () => {
      await expect(
        recordSearchLog(buildCriteria({ freeWord: null }), 5),
      ).resolves.toBeUndefined();
    });

    it("freeWord がスペースのみの場合は何も実行しない", async () => {
      await expect(
        recordSearchLog(buildCriteria({ freeWord: "   " }), 3),
      ).resolves.toBeUndefined();
    });
  });
});
