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

    // Verify MEMO section is displayed
    await expect(page.getByRole("heading", { name: "MEMO" })).toBeVisible();
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

  test("memos list page renders", async ({ page }: TestArgs) => {
    await page.goto("/memos", { waitUntil: "load" });

    // Verify page content is displayed
    await expect(page.locator("main")).toBeVisible();
  });

  test("series list page renders", async ({ page }: TestArgs) => {
    await page.goto("/series", { waitUntil: "load" });

    await expect(page.locator("main")).toBeVisible();
  });
});
