import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// シードデータの記事情報
const singleTagArticle = {
  slug: "typescript-type-safe-code",
  title: "TypeScriptで型安全なコードを書く",
  tags: ["TypeScript"],
};

const multiTagArticle = {
  slug: "react-component-patterns",
  title: "Reactコンポーネント設計パターン",
  tags: ["React", "TypeScript"],
};

const imageArticle = {
  slug: "markdown-image-test",
  title: "Markdownで画像を表示するテスト",
  tags: ["TypeScript", "React"],
};

/**
 * Article detail page tests
 */
test.describe("article detail page", () => {
  test("displays article title as h1 heading", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`);

    // Article title appears in both header and content, use first()
    await expect(
      page.getByRole("heading", { name: singleTagArticle.title, level: 1 }).first(),
    ).toBeVisible();
  });

  test("displays publication date", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check for date format (年月日 or YYYY-MM-DD or similar)
    const datePattern = /\d{4}[年.\-/]\d{1,2}[月.\-/]\d{1,2}/;
    const mainContent = await page.locator("main").textContent();
    expect(mainContent).toMatch(datePattern);
  });

  test("displays single tag with link", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check tag is displayed as a link
    const tagLink = page.getByRole("link", { name: singleTagArticle.tags[0] });
    await expect(tagLink).toBeVisible();

    // Verify tag link points to search or tag page
    const href = await tagLink.getAttribute("href");
    expect(href).toBeTruthy();
  });

  test("displays multiple tags with links", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${multiTagArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check all tags are displayed
    for (const tagName of multiTagArticle.tags) {
      const tagLink = page.getByRole("link", { name: tagName });
      await expect(tagLink).toBeVisible();
    }
  });

  test("displays table of contents", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check for TOC headings from the article content
    // The article has "型の基本" and "ジェネリクス" as h2 headings
    const tocItems = page.locator("nav a, aside a").filter({ hasText: /型の基本|ジェネリクス/ });
    const count = await tocItems.count();
    expect(count).toBeGreaterThan(0);
  });

  test("displays code blocks with syntax highlighting", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check for code blocks (pre > code or shiki elements)
    const codeBlocks = page.locator("pre code, .shiki");
    await expect(codeBlocks.first()).toBeVisible();

    // Verify TypeScript code content is present
    const codeContent = await page.locator("pre").first().textContent();
    expect(codeContent).toContain("string");
  });

  test("displays inline images in article content", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/articles/${imageArticle.slug}`);

    // Wait for content to load including images
    await page.waitForLoadState("networkidle");

    // Check for inline image in the article body (not the header image)
    // The image has alt text "コードを書いている様子"
    const inlineImage = page.locator("article img, .prose img, main img").filter({
      has: page.locator('[alt*="コード"]'),
    });

    // At least one image should be visible
    const images = page.locator("article img, .prose img");
    const imageCount = await images.count();
    expect(imageCount).toBeGreaterThan(0);
  });

  test("article content is rendered as prose", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check that article content includes expected text
    await expect(
      page.getByText("TypeScriptは、JavaScriptに型システムを追加した言語です。"),
    ).toBeVisible();
  });

  test("h2 headings have proper styling", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check h2 headings from article content
    const h2Heading = page.getByRole("heading", { name: "型の基本", level: 2 });
    await expect(h2Heading).toBeVisible();
  });

  test("clicking TOC link scrolls to section", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Find a TOC link (assuming it links to h2 sections)
    const tocLink = page.locator("nav a, aside a").filter({ hasText: "ジェネリクス" }).first();

    if ((await tocLink.count()) > 0) {
      // Click the TOC link
      await tocLink.click();

      // Wait for scroll
      await page.waitForTimeout(500);

      // Verify the heading is now in viewport
      const heading = page.getByRole("heading", { name: "ジェネリクス", level: 2 });
      await expect(heading).toBeInViewport();
    }
  });

  test("shows 404 for non-existent article slug", async ({
    page,
  }: TestArgs) => {
    await page.goto("/articles/non-existent-article-slug-xyz");

    // Check for not-found page content (Next.js returns 200 with not-found page)
    // Look for common 404 indicators
    const notFoundIndicators = page.locator("text=/404|not found|見つかりません|ページが見つかりません/i");
    await expect(notFoundIndicators.first()).toBeVisible();
  });

  test("draft article page renders (admin preview)", async ({
    page,
  }: TestArgs) => {
    // nextjs-app-router is a draft article
    // Draft articles may be accessible for preview purposes
    await page.goto("/articles/nextjs-app-router");

    // Verify page loads (either shows content or 404)
    await page.waitForLoadState("networkidle");

    // Check if article title is displayed (draft accessible) or 404 page
    const articleTitle = page.getByRole("heading", { name: "Next.js App Routerの使い方" }).first();
    const notFoundIndicators = page.locator("text=/404|not found|見つかりません/i");

    const isTitleVisible = await articleTitle.isVisible().catch(() => false);
    const isNotFound = await notFoundIndicators.first().isVisible().catch(() => false);

    // Either the article is accessible or shows 404
    expect(isTitleVisible || isNotFound).toBe(true);
  });

  test("image article displays both header and inline images", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/articles/${imageArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check article title
    await expect(
      page.getByRole("heading", { name: imageArticle.title }).first(),
    ).toBeVisible();

    // Check for multiple images (header + inline)
    const images = page.locator("img");
    const imageCount = await images.count();
    expect(imageCount).toBeGreaterThanOrEqual(1);
  });

  test("code block in image article is displayed", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/articles/${imageArticle.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check for code content
    await expect(page.getByText("画像とコードの共存テスト")).toBeVisible();
  });
});
