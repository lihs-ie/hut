import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// シードデータの公開記事情報 (下書きを除く)
const publishedArticles = [
  {
    slug: "typescript-type-safe-code",
    title: "TypeScriptで型安全なコードを書く",
    tags: ["TypeScript"],
  },
  {
    slug: "react-component-patterns",
    title: "Reactコンポーネント設計パターン",
    tags: ["React", "TypeScript"],
  },
  {
    slug: "markdown-image-test",
    title: "Markdownで画像を表示するテスト",
    tags: ["TypeScript", "React"],
  },
];

// 下書き記事
const draftArticle = {
  slug: "nextjs-app-router",
  title: "Next.js App Routerの使い方",
};

// シードデータの公開メモ情報 (下書きを除く)
const publishedMemos = [
  {
    slug: "go-tips",
    title: "Go言語のTips",
    tags: ["Go"],
  },
];

// プロフィール情報
const profile = {
  name: "Taro Yamada",
  role: "Frontend / Backend Developer",
  bio: "フルスタックエンジニア。TypeScript、React、Go、Rustを主に使用。クリーンアーキテクチャとDDDに興味があります。",
};

/**
 * Home page (top page) tests
 */
test.describe("home page", () => {
  test.describe("page structure", () => {
    test("page loads successfully", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Verify main content is displayed
      await expect(page.locator("main")).toBeVisible();
    });

    test("displays sections in correct order: ARTICLE, MEMO, Profile", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Get all sections
      const articleSection = page.getByRole("heading", { name: "ARTICLE" });
      const memoSection = page.getByRole("heading", { name: "MEMO" });
      const profileName = page.getByRole("heading", { name: profile.name });

      // All should be visible
      await expect(articleSection).toBeVisible();
      await expect(memoSection).toBeVisible();
      await expect(profileName).toBeVisible();

      // Verify order by checking bounding boxes
      const articleBox = await articleSection.boundingBox();
      const memoBox = await memoSection.boundingBox();
      const profileBox = await profileName.boundingBox();

      expect(articleBox!.y).toBeLessThan(memoBox!.y);
      expect(memoBox!.y).toBeLessThan(profileBox!.y);
    });
  });

  test.describe("article section", () => {
    test("displays ARTICLE section heading", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: "ARTICLE", level: 2 }),
      ).toBeVisible();
    });

    test("displays 'もっと見る' link pointing to /articles", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Find the "もっと見る" link in the article section
      const viewMoreLinks = page.getByRole("link", { name: /もっと見る/ });
      const firstLink = viewMoreLinks.first();

      await expect(firstLink).toBeVisible();
      const href = await firstLink.getAttribute("href");
      expect(href).toBe("/articles");
    });

    test("displays published articles count", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check that all published articles are displayed
      for (const article of publishedArticles) {
        await expect(page.getByText(article.title).first()).toBeVisible();
      }
    });

    test("does not display draft articles", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Draft article should not be visible
      await expect(page.getByText(draftArticle.title)).not.toBeVisible();
    });

    test("article cards display 記事 badge", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check for "記事" badges (at least as many as published articles)
      const badges = page.locator("text=記事");
      const count = await badges.count();
      expect(count).toBeGreaterThanOrEqual(publishedArticles.length);
    });

    test("article cards display date in YYYY/MM/DD format", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check for date format in article section
      const datePattern = /\d{4}\/\d{2}\/\d{2}/;
      const mainContent = await page.locator("main").textContent();
      expect(mainContent).toMatch(datePattern);
    });

    test("article cards display tags with # prefix", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check for tags with # prefix
      await expect(page.getByText("#TypeScript").first()).toBeVisible();
    });

    test("clicking article card navigates to article detail", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Find and click the first article
      const articleLink = page
        .getByRole("link")
        .filter({ hasText: publishedArticles[0].title })
        .first();
      await articleLink.click();

      // Verify navigation to article detail page
      await expect(page).toHaveURL(
        new RegExp(`/articles/${publishedArticles[0].slug}`),
      );
    });

    test("article cards have clickable links to detail pages", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check that article links point to correct URLs
      for (const article of publishedArticles) {
        const articleLink = page
          .getByRole("link")
          .filter({ hasText: article.title })
          .first();
        const href = await articleLink.getAttribute("href");
        expect(href).toContain(`/articles/${article.slug}`);
      }
    });

    test("displays maximum 6 articles", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Count article cards (those with "記事" badge)
      const articleBadges = page.locator("text=記事");
      const count = await articleBadges.count();

      // Should be at most 6 (maxItems default)
      expect(count).toBeLessThanOrEqual(6);
    });
  });

  test.describe("memo section", () => {
    test("displays MEMO section heading", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: "MEMO", level: 2 }),
      ).toBeVisible();
    });

    test("displays 'もっと見る' link pointing to /memos", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Find the "もっと見る" link that points to /memos
      const viewMoreLinks = page.getByRole("link", { name: /もっと見る/ });
      let foundMemosLink = false;

      const count = await viewMoreLinks.count();
      for (let i = 0; i < count; i++) {
        const href = await viewMoreLinks.nth(i).getAttribute("href");
        if (href === "/memos") {
          foundMemosLink = true;
          break;
        }
      }

      expect(foundMemosLink).toBe(true);
    });

    test("displays published memos", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check that published memo is displayed
      for (const memo of publishedMemos) {
        await expect(page.getByText(memo.title).first()).toBeVisible();
      }
    });

    test("memo cards display メモ badge", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check for "メモ" badge
      const badges = page.locator("text=メモ");
      const count = await badges.count();
      expect(count).toBeGreaterThanOrEqual(publishedMemos.length);
    });

    test("memo cards display tags with # prefix", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check for Go tag with # prefix
      await expect(page.getByText("#Go").first()).toBeVisible();
    });

    test("clicking memo card navigates to memo detail", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Find and click the memo
      const memoLink = page
        .getByRole("link")
        .filter({ hasText: publishedMemos[0].title })
        .first();
      await memoLink.click();

      // Verify navigation to memo detail page
      await expect(page).toHaveURL(
        new RegExp(`/memos/${publishedMemos[0].slug}`),
      );
    });

    test("memo cards have clickable links to detail pages", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check that memo links point to correct URLs
      for (const memo of publishedMemos) {
        const memoLink = page
          .getByRole("link")
          .filter({ hasText: memo.title })
          .first();
        const href = await memoLink.getAttribute("href");
        expect(href).toContain(`/memos/${memo.slug}`);
      }
    });
  });

  test.describe("profile card", () => {
    test("displays profile name", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: profile.name, level: 3 }),
      ).toBeVisible();
    });

    test("displays role text", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      await expect(page.getByText(profile.role)).toBeVisible();
    });

    test("displays bio text", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      await expect(page.getByText(profile.bio)).toBeVisible();
    });

    test("displays 'プロフィールを見る' link pointing to /about", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      const profileLink = page.getByRole("link", {
        name: /詳しいプロフィールを見る/,
      });
      await expect(profileLink).toBeVisible();

      const href = await profileLink.getAttribute("href");
      expect(href).toBe("/about");
    });

    test("clicking profile link navigates to about page", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      const profileLink = page.getByRole("link", {
        name: /詳しいプロフィールを見る/,
      });
      await profileLink.click();

      await expect(page).toHaveURL(/\/about/);
    });

    test("displays avatar image", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check for avatar image (img element with alt text matching profile name)
      const avatar = page.locator(`img[alt="${profile.name}"]`);
      await expect(avatar).toBeVisible();
    });
  });

  test.describe("content card details", () => {
    test("article cards are clickable links", async ({ page }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Each article title should be within a link
      for (const article of publishedArticles) {
        const link = page
          .getByRole("link")
          .filter({ hasText: article.title })
          .first();
        await expect(link).toBeVisible();
      }
    });

    test("content cards display article element", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // HomeContentCard renders article elements
      const articles = page.locator("article");
      const count = await articles.count();

      // Should have cards for articles + memos
      expect(count).toBeGreaterThanOrEqual(
        publishedArticles.length + publishedMemos.length,
      );
    });

    test("content cards have proper semantic structure", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Check section elements exist for ARTICLE and MEMO sections
      const sections = page.locator("section");
      const sectionCount = await sections.count();

      // At least 2 sections (ARTICLE, MEMO)
      expect(sectionCount).toBeGreaterThanOrEqual(2);
    });
  });

  test.describe("view more links", () => {
    test("clicking articles もっと見る navigates to /articles", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Click the first もっと見る link (should be for articles)
      const viewMoreLink = page
        .getByRole("link", { name: /もっと見る/ })
        .first();
      await viewMoreLink.click();

      await expect(page).toHaveURL(/\/articles/);
    });

    test("clicking memos もっと見る navigates to /memos", async ({
      page,
    }: TestArgs) => {
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Find the もっと見る link for memos section
      const memoSection = page.locator("section").filter({
        has: page.getByRole("heading", { name: "MEMO" }),
      });
      const viewMoreLink = memoSection.getByRole("link", {
        name: /もっと見る/,
      });
      await viewMoreLink.click();

      await expect(page).toHaveURL(/\/memos/);
    });
  });

  test.describe("responsiveness", () => {
    test("page is viewable on mobile viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 375, height: 667 });
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Key elements should still be visible
      await expect(
        page.getByRole("heading", { name: "ARTICLE" }),
      ).toBeVisible();
      await expect(page.getByRole("heading", { name: "MEMO" })).toBeVisible();
      await expect(
        page.getByRole("heading", { name: profile.name }),
      ).toBeVisible();
    });

    test("page is viewable on tablet viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 768, height: 1024 });
      await page.goto("/");
      await page.waitForLoadState("networkidle");

      // Key elements should still be visible
      await expect(
        page.getByRole("heading", { name: "ARTICLE" }),
      ).toBeVisible();
      await expect(page.getByRole("heading", { name: "MEMO" })).toBeVisible();
      await expect(
        page.getByRole("heading", { name: profile.name }),
      ).toBeVisible();
    });
  });
});
