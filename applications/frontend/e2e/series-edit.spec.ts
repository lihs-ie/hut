import { expect, type Page, test } from "@playwright/test";

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
});
