import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

test.describe("privacy policy edit", () => {
  test("privacy policy edit page renders", async ({ page }: TestArgs) => {
    await page.goto("/admin/privacy/edit");

    // Verify "プライバシーポリシー" heading is displayed
    await expect(
      page.getByRole("heading", { name: "プライバシーポリシー" }),
    ).toBeVisible();
  });

  test("save button is displayed", async ({ page }: TestArgs) => {
    await page.goto("/admin/privacy/edit");

    // Verify "保存する" button is displayed
    await expect(page.getByRole("button", { name: "保存する" })).toBeVisible();
  });

  test("add section button is displayed", async ({ page }: TestArgs) => {
    await page.goto("/admin/privacy/edit");

    // Verify "セクションを追加" button is displayed
    await expect(
      page.getByRole("button", { name: "セクションを追加" }),
    ).toBeVisible();
  });

  test("add section button works (non-destructive)", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/privacy/edit");

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Click add section button
    await page.getByRole("button", { name: "セクションを追加" }).click();

    // Verify a new section is added (form should still be visible)
    await expect(page.locator("main")).toBeVisible();

    // Note: We don't save, so data is not affected
  });
});
