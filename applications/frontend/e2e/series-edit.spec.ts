import { expect, type Page, test } from "@playwright/test";
import {
  getSeriesIdentifierBySlug,
  getContentTokenIndex,
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
    await page.goto(`/series/${series.slug}/edit`, { waitUntil: "load" });

    await expect(page.locator("main")).toBeVisible({ timeout: 15000 });
  });

  test("displays page title 連載を編集", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`, { waitUntil: "load" });

    await expect(
      page.getByRole("heading", { name: "連載を編集" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("existing title is prefilled in input", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`, { waitUntil: "load" });

    await expect(
      page.getByPlaceholder("連載タイトルを入力"),
    ).toHaveValue(series.title, { timeout: 15000 });
  });

  test("save button is present", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`, { waitUntil: "load" });

    await expect(
      page.getByRole("button", { name: "保存" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("displays chapter management section", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`, { waitUntil: "load" });

    await expect(
      page.getByRole("heading", { name: "チャプター管理" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("displays add chapter link", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`, { waitUntil: "load" });

    await expect(
      page.getByRole("link", { name: /チャプターを追加/ }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("displays chapter list with edit links", async ({ page }: TestArgs) => {
    await page.goto(`/series/${series.slug}/edit`, { waitUntil: "load" });

    await expect(
      page.getByText("第1章: Rustの基礎").first(),
    ).toBeVisible({ timeout: 15000 });

    await expect(
      page.getByRole("link", { name: "編集" }).first(),
    ).toBeVisible({ timeout: 15000 });
  });
});

test.describe("series search token verification", () => {
  test("search tokens exist for published series", async () => {
    const seriesIdentifier = await getSeriesIdentifierBySlug(series.slug);

    if (seriesIdentifier === undefined) {
      test.skip(true, "seed series not found in Firestore");
      return;
    }

    const tokenIndex = await getContentTokenIndex(
      "series",
      seriesIdentifier,
    );

    expect(tokenIndex).toBeDefined();

    if (tokenIndex === undefined) {
      return;
    }

    expect(tokenIndex.tokens.length).toBeGreaterThan(0);
  });
});
