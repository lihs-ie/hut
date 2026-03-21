import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const seriesSlug = "rust-system-programming";

test.describe("chapter create page", () => {
  test("page is accessible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/new`);

    await expect(page.locator("main")).toBeVisible({ timeout: 15000 });
  });

  test("CodeMirror editor is visible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/new`);

    await expect(
      page.locator(".cm-content"),
    ).toBeVisible({ timeout: 15000 });
  });

  test("save button is present", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/new`);

    await expect(
      page.getByRole("button", { name: /保存|下書き保存|公開する/ }),
    ).toBeVisible({ timeout: 15000 });
  });
});
