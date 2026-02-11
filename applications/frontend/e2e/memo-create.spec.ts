import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

/**
 * Memo creation tests - require authentication
 */
test.describe("memo creation", () => {
  test("memo creation page renders", async ({ page }: TestArgs) => {
    await page.goto("/memos/new");

    // Verify form is displayed
    await expect(page.getByPlaceholder("Enter title...")).toBeVisible();

    // Verify create button is displayed
    await expect(page.getByRole("button", { name: "メモを作成" })).toBeVisible();
  });

  test("create button is disabled when title is empty", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new");

    // Verify create button is disabled when title is empty
    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeDisabled();
  });

  test("create button is enabled when title is filled", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new");

    // Fill in the title
    await page.getByPlaceholder("Enter title...").fill("テストメモ");

    // Verify create button is now enabled
    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeEnabled();
  });

  test("status dropdown select is displayed", async ({ page }: TestArgs) => {
    await page.goto("/memos/new");

    // Verify status dropdown is displayed (default is "下書き")
    await expect(page.getByText("下書き")).toBeVisible();
  });
});
