import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// Use empty storage state to test unauthenticated state
test.use({ storageState: { cookies: [], origins: [] } });

/**
 * Login page tests - require unauthenticated state
 */
test.describe("login page", () => {
  test("login page renders with Google button", async ({ page }: TestArgs) => {
    await page.goto("/admin/login");

    // Verify "管理者ログイン" heading is displayed
    await expect(
      page.getByRole("heading", { name: "管理者ログイン" }),
    ).toBeVisible();

    // Verify description text is displayed
    await expect(
      page.getByText("Google アカウントでログインしてください"),
    ).toBeVisible();

    // Verify Google login button is displayed
    await expect(
      page.getByRole("button", { name: /Sign in with Google/ }),
    ).toBeVisible();
  });

  test("allowed account hint is displayed", async ({ page }: TestArgs) => {
    await page.goto("/admin/login");

    // Verify the hint about allowed accounts is displayed
    await expect(
      page.getByText("許可された Google アカウントのみログインできます"),
    ).toBeVisible();
  });
});
