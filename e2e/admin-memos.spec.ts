import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

/**
 * Memo management tests - require authentication
 */
test.describe("memo management", () => {
  test("memo management page renders", async ({ page }: TestArgs) => {
    await page.goto("/admin/memos");

    // Verify "メモの管理" heading is displayed
    await expect(
      page.getByRole("heading", { name: "メモの管理" }),
    ).toBeVisible();
  });

  test("new memo creation button exists", async ({ page }: TestArgs) => {
    await page.goto("/admin/memos");

    // Verify "新規作成" button is displayed
    await expect(page.getByRole("button", { name: "新規作成" })).toBeVisible();
  });

  test("search input field exists", async ({ page }: TestArgs) => {
    await page.goto("/admin/memos");

    // Verify search input is displayed
    await expect(page.getByPlaceholder("タイトルを入力")).toBeVisible();
  });

  test("memo list is displayed", async ({ page }: TestArgs) => {
    await page.goto("/admin/memos");

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Verify main content area is displayed
    await expect(page.locator("main")).toBeVisible();
  });

  test("new memo creation button navigates to create page", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/memos");

    // Click new memo button
    await page.getByRole("button", { name: "新規作成" }).click();

    // Verify navigation to memo creation page
    await page.waitForURL("/memos/new", { timeout: 10000 });
  });
});
