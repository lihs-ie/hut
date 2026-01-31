import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

/**
 * Tag management tests - require authentication
 */
test.describe.serial("tag management", () => {
  test("tag list page renders", async ({ page }: TestArgs) => {
    await page.goto("/admin/tags");

    // Verify "タグ管理" heading is displayed
    await expect(page.getByRole("heading", { name: "タグ管理" })).toBeVisible();
  });

  test("new tag creation link exists", async ({ page }: TestArgs) => {
    await page.goto("/admin/tags");

    // Verify "新規作成" link is displayed
    await expect(page.getByRole("link", { name: "新規作成" })).toBeVisible();
  });

  test("tag creation page renders", async ({ page }: TestArgs) => {
    await page.goto("/admin/tags/new");

    // Verify "新規タグ作成" heading is displayed
    await expect(
      page.getByRole("heading", { name: "新規タグ作成" }),
    ).toBeVisible();

    // Verify back link is displayed
    await expect(
      page.getByRole("link", { name: "タグ一覧に戻る" }),
    ).toBeVisible();
  });

  test("tag creation form elements are displayed", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/tags/new");

    // Verify form elements
    await expect(page.getByPlaceholder("例: Next.js")).toBeVisible();
    await expect(
      page.getByPlaceholder("https://example.com/logo.png"),
    ).toBeVisible();

    // Verify buttons
    await expect(page.getByRole("button", { name: "作成する" })).toBeVisible();
    await expect(
      page.getByRole("button", { name: "キャンセル" }),
    ).toBeVisible();
  });

  test("create button is disabled when name is empty", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/tags/new");

    // Verify create button is disabled when name is empty
    const createButton = page.getByRole("button", { name: "作成する" });
    await expect(createButton).toBeDisabled();
  });

  test("create button is enabled when name and logo are filled", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/tags/new");

    // Fill in the name and logo
    await page.getByPlaceholder("例: Next.js").fill("テストタグ");
    await page
      .getByPlaceholder("https://example.com/logo.png")
      .fill("https://example.com/test.svg");

    // Verify create button is now enabled
    const createButton = page.getByRole("button", { name: "作成する" });
    await expect(createButton).toBeEnabled();
  });

  test("cancel button navigates back to tag list", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/tags/new");

    // Click cancel button
    await page.getByRole("button", { name: "キャンセル" }).click();

    // Verify navigation to tag list
    await page.waitForURL("/admin/tags", { timeout: 10000 });
    await expect(page.getByRole("heading", { name: "タグ管理" })).toBeVisible();
  });
});
