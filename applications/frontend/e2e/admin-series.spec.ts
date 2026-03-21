import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const series = {
  slug: "rust-system-programming",
  title: "Rustで学ぶシステムプログラミング",
};

test.describe("series management", () => {
  test("series management page renders", async ({ page }: TestArgs) => {
    await page.goto("/admin/series");
    await page.waitForLoadState("networkidle");

    await expect(
      page.getByRole("heading", { name: "シリーズの管理" }),
    ).toBeVisible();
  });

  test("series list is displayed", async ({ page }: TestArgs) => {
    await page.goto("/admin/series");
    await page.waitForLoadState("networkidle");

    await expect(page.locator("main")).toBeVisible();
  });

  test("displays seeded series title", async ({ page }: TestArgs) => {
    await page.goto("/admin/series");
    await page.waitForLoadState("networkidle");

    await expect(page.getByText(series.title).first()).toBeVisible();
  });
});
