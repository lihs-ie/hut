import { expect, test } from "@playwright/test";

test.describe("Article-only search", () => {
  test("公開記事を検索できる", async ({ page }) => {
    await page.goto("/search?freeWord=TypeScript", { waitUntil: "load" });

    await expect(page.getByText("TypeScriptで型安全なコードを書く").first()).toBeVisible({
      timeout: 15000,
    });
    await expect(page.getByRole("button", { name: "メモ" })).toHaveCount(0);
    await expect(page.getByRole("button", { name: "連載" })).toHaveCount(0);
  });

  test("旧 Memo 指定で記事以外を返さない", async ({ page }) => {
    await page.goto("/search?freeWord=TypeScript&type=memo", { waitUntil: "load" });

    await expect(page.getByText("検索結果：0件")).toBeVisible({ timeout: 15000 });
    await expect(page.locator("article")).toHaveCount(0);
  });
});
