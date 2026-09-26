import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

/**
 * Public pages tests - no authentication required for viewing
 */
test.describe("public pages", () => {
  test("home page renders with content sections", async ({
    page,
  }: TestArgs) => {
    await page.goto("/", { waitUntil: "load" });

    // Verify ARTICLE section is displayed
    await expect(page.getByRole("heading", { name: "ARTICLE" })).toBeVisible();

    await expect(page.getByRole("heading", { name: "MEMO" })).toHaveCount(0);
    await expect(page.getByRole("heading", { name: "SERIES" })).toHaveCount(0);
  });

  test("about page renders", async ({ page }: TestArgs) => {
    await page.goto("/about", { waitUntil: "load" });

    // Verify "About Me" heading is displayed
    await expect(page.getByRole("heading", { name: "About Me" })).toBeVisible();
  });

  test("privacy policy page renders", async ({ page }: TestArgs) => {
    await page.goto("/privacy", { waitUntil: "load" });

    // Verify page content is displayed
    await expect(page.locator("main")).toBeVisible();
  });

  test("search page renders", async ({ page }: TestArgs) => {
    await page.goto("/search", { waitUntil: "load" });

    // Verify filter area is displayed
    await expect(page.locator("main")).toBeVisible();
  });

  test("articles list page renders", async ({ page }: TestArgs) => {
    await page.goto("/articles", { waitUntil: "load" });

    // Verify page content is displayed
    await expect(page.locator("main")).toBeVisible();
  });

  test("memo と series の公開ページは存在しない", async ({ page }: TestArgs) => {
    const memoResponse = await page.goto("/memos", { waitUntil: "load" });
    expect(memoResponse?.status()).toBe(404);
    const seriesResponse = await page.goto("/series", { waitUntil: "load" });
    expect(seriesResponse?.status()).toBe(404);
  });
});
