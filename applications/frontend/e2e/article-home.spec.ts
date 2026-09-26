import { expect, test } from "@playwright/test";

test.describe("Article-only home", () => {
  test("記事の公開導線を表示し、Memo と Series は表示しない", async ({ page }) => {
    await page.goto("/", { waitUntil: "load" });

    const articleSection = page.locator("section").filter({
      has: page.getByRole("heading", { name: "ARTICLE" }),
    });
    await expect(articleSection).toBeVisible();
    await expect(articleSection.getByRole("link", { name: /もっと見る/ })).toHaveAttribute(
      "href",
      "/articles",
    );
    await expect(page.getByRole("heading", { name: "MEMO" })).toHaveCount(0);
    await expect(page.getByRole("heading", { name: "SERIES" })).toHaveCount(0);
    await expect(page.getByRole("link", { name: "Memos" })).toHaveCount(0);
  });
});
