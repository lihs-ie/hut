import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const publishedArticlesNewestFirst = [
  {
    slug: "mermaid-all-diagrams",
    title: "Mermaid図 全サンプル集",
  },
  {
    slug: "mermaid-diagram-test",
    title: "Mermaid図を使った技術ドキュメント",
  },
  {
    slug: "markdown-image-test",
    title: "Markdownで画像を表示するテスト",
  },
  {
    slug: "react-component-patterns",
    title: "Reactコンポーネント設計パターン",
  },
  {
    slug: "typescript-type-safe-code",
    title: "TypeScriptで型安全なコードを書く",
  },
];

const draftArticle = {
  slug: "nextjs-app-router",
  title: "Next.js App Routerの使い方",
};

test.describe("articles list page", () => {
  test.describe("page structure", () => {
    test("page loads successfully", async ({ page }: TestArgs) => {
      await page.goto("/articles", { waitUntil: "load" });

      await expect(page.locator("main")).toBeVisible();
    });

    test("displays ARTICLE section heading", async ({ page }: TestArgs) => {
      await page.goto("/articles", { waitUntil: "load" });

      await expect(
        page.getByRole("heading", { name: "ARTICLE" }),
      ).toBeVisible();
    });
  });

  test.describe("display order", () => {
    test("first article card in DOM is the newest article", async ({
      page,
    }: TestArgs) => {
      await page.goto("/articles", { waitUntil: "load" });

      const articleCardLinks = page
        .locator("main")
        .getByRole("link")
        .filter({ has: page.locator("article") });

      await expect(articleCardLinks.first()).toBeVisible();

      const firstLinkHref = await articleCardLinks
        .first()
        .getAttribute("href");
      expect(firstLinkHref).toContain(
        `/articles/${publishedArticlesNewestFirst[0].slug}`,
      );

      const firstCardTitle = articleCardLinks
        .first()
        .getByRole("heading", { level: 2 });
      await expect(firstCardTitle).toHaveText(
        publishedArticlesNewestFirst[0].title,
      );
    });

    test("article cards appear in newest first order in DOM", async ({
      page,
    }: TestArgs) => {
      await page.goto("/articles", { waitUntil: "load" });

      const articleCardLinks = page
        .locator("main")
        .getByRole("link")
        .filter({ has: page.locator("article") });

      await expect(articleCardLinks.first()).toBeVisible();

      const hrefs = await articleCardLinks.evaluateAll((elements) =>
        elements.map((element) => element.getAttribute("href") ?? ""),
      );

      const orderedSlugs = publishedArticlesNewestFirst.map(
        (article) => article.slug,
      );
      const domIndices = orderedSlugs.map((slug) =>
        hrefs.findIndex((href) => href.includes(`/articles/${slug}`)),
      );

      for (const index of domIndices) {
        expect(index).toBeGreaterThanOrEqual(0);
      }

      for (let position = 1; position < domIndices.length; position += 1) {
        expect(domIndices[position - 1]).toBeLessThan(domIndices[position]);
      }
    });

    test("newest article appears above oldest article visually", async ({
      page,
    }: TestArgs) => {
      await page.goto("/articles", { waitUntil: "load" });

      const oldestArticle =
        publishedArticlesNewestFirst[publishedArticlesNewestFirst.length - 1];

      const newestCard = page
        .locator("main")
        .getByRole("link")
        .filter({ hasText: publishedArticlesNewestFirst[0].title })
        .first();
      const oldestCard = page
        .locator("main")
        .getByRole("link")
        .filter({ hasText: oldestArticle.title })
        .first();

      await expect(newestCard).toBeVisible();
      await expect(oldestCard).toBeVisible();

      const newestBox = await newestCard.boundingBox();
      const oldestBox = await oldestCard.boundingBox();

      expect(newestBox).not.toBeNull();
      expect(oldestBox).not.toBeNull();
      expect(newestBox!.y).toBeLessThan(oldestBox!.y);
    });
  });

  test.describe("published content", () => {
    test("displays all published articles", async ({ page }: TestArgs) => {
      await page.goto("/articles", { waitUntil: "load" });

      for (const article of publishedArticlesNewestFirst) {
        await expect(page.getByText(article.title).first()).toBeVisible();
      }
    });

    test("does not display draft articles", async ({ page }: TestArgs) => {
      await page.goto("/articles", { waitUntil: "load" });

      await expect(page.getByText(draftArticle.title)).not.toBeVisible();
    });
  });

  test.describe("responsiveness", () => {
    test("page is viewable on mobile viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 375, height: 667 });
      await page.goto("/articles", { waitUntil: "load" });

      await expect(
        page.getByRole("heading", { name: "ARTICLE" }),
      ).toBeVisible();
    });
  });
});
