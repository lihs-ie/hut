import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

test.describe("series create page", () => {
  test("page is accessible", async ({ page }: TestArgs) => {
    await page.goto("/series/new");

    await expect(page.locator("main")).toBeVisible({ timeout: 15000 });
  });

  test("displays page title 連載を作成", async ({ page }: TestArgs) => {
    await page.goto("/series/new");

    await expect(
      page.getByRole("heading", { name: "連載を作成" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("title input field is present", async ({ page }: TestArgs) => {
    await page.goto("/series/new");

    await expect(
      page.getByPlaceholder("連載タイトルを入力"),
    ).toBeVisible({ timeout: 15000 });
  });

  test("slug input field is present", async ({ page }: TestArgs) => {
    await page.goto("/series/new");

    await expect(
      page.getByPlaceholder("series-slug"),
    ).toBeVisible({ timeout: 15000 });
  });

  test("save button is present", async ({ page }: TestArgs) => {
    await page.goto("/series/new");

    await expect(
      page.getByRole("button", { name: "保存" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("publish status checkbox is present", async ({ page }: TestArgs) => {
    await page.goto("/series/new");

    await expect(
      page.getByRole("checkbox"),
    ).toBeVisible({ timeout: 15000 });
  });
});
