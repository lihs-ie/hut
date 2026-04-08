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
    await page.goto("/admin/series", { waitUntil: "load" });

    await expect(
      page.getByRole("heading", { name: "連載の管理" }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("series list is displayed", async ({ page }: TestArgs) => {
    await page.goto("/admin/series", { waitUntil: "load" });

    await expect(page.locator("main")).toBeVisible({ timeout: 15000 });
  });

  test("displays seeded series title", async ({ page }: TestArgs) => {
    await page.goto("/admin/series", { waitUntil: "load" });

    await expect(
      page.getByText(series.title).first(),
    ).toBeVisible({ timeout: 15000 });
  });
});
