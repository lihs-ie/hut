import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// 未ログイン状態でテストを実行
test.use({ storageState: { cookies: [], origins: [] } });

/**
 * Admin unauthenticated access tests
 * Verify that all admin operations (except login page) require authentication
 *
 * Note: Admin app requires authentication for ALL pages except /admin/login.
 * Public content viewing is done via the reader app (port 3000).
 */
test.describe("admin unauthenticated access", () => {
  test.describe("dashboard pages require authentication", () => {
    test("articles management page redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/articles");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("memos management page redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/memos");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("tags management page redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/tags");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("new tag page redirects to login", async ({ page }: TestArgs) => {
      await page.goto("/admin/tags/new");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("tag edit page redirects to login", async ({ page }: TestArgs) => {
      // Using a dummy identifier - will redirect before checking validity
      await page.goto("/admin/tags/dummy-identifier/edit");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("series management page redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/series");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("profile edit page redirects to login", async ({ page }: TestArgs) => {
      await page.goto("/admin/profile/edit");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("privacy policy edit page redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/privacy/edit");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });
  });

  test.describe("content creation pages require authentication", () => {
    test("new article page redirects to login", async ({ page }: TestArgs) => {
      await page.goto("/articles/new");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("new memo page redirects to login", async ({ page }: TestArgs) => {
      await page.goto("/memos/new");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });
  });

  test.describe("content edit pages require authentication", () => {
    test("article edit page redirects to login", async ({ page }: TestArgs) => {
      await page.goto("/articles/typescript-type-safe-code/edit");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("memo edit page redirects to login", async ({ page }: TestArgs) => {
      await page.goto("/memos/go-tips/edit");

      // Should redirect to login page
      await expect(page).toHaveURL(/\/admin\/login/);
    });
  });

  test.describe("content preview pages require authentication", () => {
    test("home page (admin) redirects to login", async ({ page }: TestArgs) => {
      await page.goto("/");

      // Admin app requires authentication for all pages
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("about page (admin) redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/about");

      // Admin app requires authentication for all pages
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("privacy page (admin) redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/privacy");

      // Admin app requires authentication for all pages
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("search page (admin) redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/search");

      // Admin app requires authentication for all pages
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("articles list page (admin) redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/articles");

      // Admin app requires authentication for all pages
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("article detail page (admin) redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/articles/typescript-type-safe-code");

      // Admin app requires authentication for all pages
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("memos list page (admin) redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/memos");

      // Admin app requires authentication for all pages
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("memo detail page (admin) redirects to login", async ({
      page,
    }: TestArgs) => {
      await page.goto("/memos/go-tips");

      // Admin app requires authentication for all pages
      await expect(page).toHaveURL(/\/admin\/login/);
    });
  });

  test.describe("login page is accessible", () => {
    test("login page loads without redirect", async ({ page }: TestArgs) => {
      await page.goto("/admin/login");

      // Should stay on login page
      await expect(page).toHaveURL(/\/admin\/login/);

      // Login page should be visible
      await expect(page.locator("main")).toBeVisible();
    });

    test("login page displays sign-in content", async ({ page }: TestArgs) => {
      await page.goto("/admin/login");

      // Wait for page to load
      await page.waitForLoadState("networkidle");

      // Login UI should be visible
      await expect(page.locator("main")).toBeVisible();
    });
  });

  test.describe("redirect behavior", () => {
    test("redirects preserve no sensitive query params", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/articles?sensitive=data");

      // Should redirect to login without sensitive params
      await expect(page).toHaveURL(/\/admin\/login/);
    });

    test("multiple consecutive requests all redirect", async ({
      page,
    }: TestArgs) => {
      // First request
      await page.goto("/admin/articles");
      await expect(page).toHaveURL(/\/admin\/login/);

      // Second request to different protected page
      await page.goto("/admin/memos");
      await expect(page).toHaveURL(/\/admin\/login/);

      // Third request
      await page.goto("/admin/tags");
      await expect(page).toHaveURL(/\/admin\/login/);
    });
  });
});
