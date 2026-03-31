import { expect, type Page, test } from "@playwright/test";
import {
  getSeriesIdentifierBySlug,
  waitForSearchTokens,
} from "./helpers/search-token";

type TestArgs = {
  page: Page;
};

const series = {
  slug: "rust-system-programming",
  title: "Rustで学ぶシステムプログラミング",
};

test.describe("series edit page", () => {
  test("page is accessible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`);

    await expect(page.locator("main")).toBeVisible({ timeout: 15000 });
  });

  test("displays page title 連載を編集", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`);

    await expect(
      page.getByRole("heading", { name: "連載を編集" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("existing title is prefilled in input", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`);

    await expect(
      page.getByPlaceholder("連載タイトルを入力"),
    ).toHaveValue(series.title, { timeout: 15000 });
  });

  test("save button is present", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`);

    await expect(
      page.getByRole("button", { name: "保存" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("displays chapter management section", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`);

    await expect(
      page.getByRole("heading", { name: "チャプター管理" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("displays add chapter link", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`);

    await expect(
      page.getByRole("link", { name: /チャプターを追加/ }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("displays chapter list with edit links", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`);

    await expect(
      page.getByText("第1章: Rustの基礎").first(),
    ).toBeVisible({ timeout: 15000 });

    await expect(
      page.getByRole("link", { name: "編集" }).first(),
    ).toBeVisible({ timeout: 15000 });
  });
});

test.describe("series search token verification", () => {
  test("search tokens exist for published series", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`);
    await page.waitForLoadState("networkidle");

    const seriesIdentifier = await getSeriesIdentifierBySlug(series.slug);

    if (seriesIdentifier === undefined) {
      throw new Error(`Series identifier not found for slug: ${series.slug}`);
    }

    const tokenIndex = await waitForSearchTokens(
      "series",
      seriesIdentifier,
      30000,
    );

    if (tokenIndex === undefined) {
      throw new Error(`Token index not found for series: ${seriesIdentifier}`);
    }

    expect(tokenIndex.tokens.length).toBeGreaterThan(0);
  });
});
