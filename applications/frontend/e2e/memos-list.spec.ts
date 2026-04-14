import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const publishedMemosNewestFirst = [
  {
    slug: "go-tips",
    title: "Go言語のTips",
  },
];

const draftMemo = {
  slug: "typescript-config",
  title: "TypeScript設定メモ",
};

test.describe("memos list page", () => {
  test.describe("page structure", () => {
    test("page loads successfully", async ({ page }: TestArgs) => {
      await page.goto("/memos", { waitUntil: "load" });

      await expect(page.locator("main")).toBeVisible();
    });

    test("displays MEMO section heading", async ({ page }: TestArgs) => {
      await page.goto("/memos", { waitUntil: "load" });

      await expect(
        page.getByRole("heading", { name: "MEMO" }),
      ).toBeVisible();
    });
  });

  test.describe("display order", () => {
    test("first memo card in DOM is the newest memo", async ({
      page,
    }: TestArgs) => {
      await page.goto("/memos", { waitUntil: "load" });

      const memoCardLinks = page
        .locator("main")
        .getByRole("link")
        .filter({ has: page.locator("article") });

      await expect(memoCardLinks.first()).toBeVisible();

      const firstLinkHref = await memoCardLinks
        .first()
        .getAttribute("href");
      expect(firstLinkHref).toContain(
        `/memos/${publishedMemosNewestFirst[0].slug}`,
      );

      const firstCardTitle = memoCardLinks
        .first()
        .getByRole("heading", { level: 2 });
      await expect(firstCardTitle).toHaveText(
        publishedMemosNewestFirst[0].title,
      );
    });
  });

  test.describe("published content", () => {
    test("displays all published memos", async ({ page }: TestArgs) => {
      await page.goto("/memos", { waitUntil: "load" });

      for (const memo of publishedMemosNewestFirst) {
        await expect(page.getByText(memo.title).first()).toBeVisible();
      }
    });

    test("does not display draft memos", async ({ page }: TestArgs) => {
      await page.goto("/memos", { waitUntil: "load" });

      await expect(page.getByText(draftMemo.title)).not.toBeVisible();
    });
  });

  test.describe("responsiveness", () => {
    test("page is viewable on mobile viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 375, height: 667 });
      await page.goto("/memos", { waitUntil: "load" });

      await expect(
        page.getByRole("heading", { name: "MEMO" }),
      ).toBeVisible();
    });
  });
});
