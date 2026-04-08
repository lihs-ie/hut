import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

/**
 * Memo creation tests - require authentication
 */
test.describe("memo creation", () => {
  test("memo creation page renders", async ({ page }: TestArgs) => {
    await page.goto("/memos/new", { waitUntil: "load" });

    await expect(page.getByPlaceholder("Enter title...")).toBeVisible();
    await expect(page.getByPlaceholder("Enter slug...")).toBeVisible();
    await expect(page.getByRole("button", { name: "メモを作成" })).toBeVisible();
  });

  test("create button is disabled when title and slug are empty", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new", { waitUntil: "load" });

    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeDisabled();
  });

  test("create button is disabled when only title is filled", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new", { waitUntil: "load" });

    await page.getByPlaceholder("Enter title...").fill("テストメモ");

    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeDisabled();
  });

  test("create button is disabled when only slug is filled", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new", { waitUntil: "load" });

    await page.getByPlaceholder("Enter slug...").fill("test-slug");

    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeDisabled();
  });

  test("create button is enabled when title and slug are filled", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new", { waitUntil: "load" });

    await page.getByPlaceholder("Enter title...").fill("テストメモ");
    await page.getByPlaceholder("Enter slug...").fill("test-slug");

    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeEnabled();
  });

  test("create button is disabled when slug contains invalid characters", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new", { waitUntil: "load" });

    await page.getByPlaceholder("Enter title...").fill("テストメモ");
    await page.getByPlaceholder("Enter slug...").fill("INVALID_SLUG!");

    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeDisabled();
  });

  test("status dropdown select is displayed", async ({ page }: TestArgs) => {
    await page.goto("/memos/new", { waitUntil: "load" });

    await expect(page.getByText("下書き")).toBeVisible();
  });
});
