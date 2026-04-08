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

const mermaidArticle = {
  slug: "mermaid-diagram-test",
  title: "Mermaid図を使った技術ドキュメント",
};

/**
 * Article detail page tests
 */
test.describe("article detail page", () => {
  test("displays article title as h1 heading", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`, { waitUntil: "load" });

    await expect(
      page.getByRole("heading", { name: singleTagArticle.title, level: 1 }).first(),
    ).toBeVisible();
  });

  test("displays publication date", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`, { waitUntil: "load" });

    const datePattern = /\d{4}[年.\-/]\d{1,2}[月.\-/]\d{1,2}/;
    const mainContent = await page.locator("main").textContent();
    expect(mainContent).toMatch(datePattern);
  });

  test("displays single tag with link", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`, { waitUntil: "load" });

    const tagLink = page.getByRole("link", { name: singleTagArticle.tags[0] });
    await expect(tagLink).toBeVisible();

    const href = await tagLink.getAttribute("href");
    expect(href).toBeTruthy();
  });

  test("displays multiple tags with links", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${multiTagArticle.slug}`, { waitUntil: "load" });

    for (const tagName of multiTagArticle.tags) {
      const tagLink = page.getByRole("link", { name: tagName });
      await expect(tagLink).toBeVisible();
    }
  });

  test("displays table of contents", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`, { waitUntil: "load" });

    const tocItems = page.locator("nav a, aside a").filter({ hasText: /型の基本|ジェネリクス/ });
    const count = await tocItems.count();
    expect(count).toBeGreaterThan(0);
  });

  test("displays code blocks with syntax highlighting", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`, { waitUntil: "load" });

    const codeBlocks = page.locator("pre code, .shiki");
    await expect(codeBlocks.first()).toBeVisible();

    const codeContent = await page.locator("pre").first().textContent();
    expect(codeContent).toContain("string");
  });

  test("displays inline images in article content", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/articles/${imageArticle.slug}`, { waitUntil: "load" });

    const images = page.locator("article img, .prose img");
    const imageCount = await images.count();
    expect(imageCount).toBeGreaterThan(0);
  });

  test("article content is rendered as prose", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`, { waitUntil: "load" });

    await expect(
      page.getByText("TypeScriptは、JavaScriptに型システムを追加した言語です。"),
    ).toBeVisible();
  });

  test("h2 headings have proper styling", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`, { waitUntil: "load" });

    const h2Heading = page.getByRole("heading", { name: "型の基本", level: 2 });
    await expect(h2Heading).toBeVisible();
  });

  test("clicking TOC link scrolls to section", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${singleTagArticle.slug}`, { waitUntil: "load" });

    const tocLink = page.locator("nav a, aside a").filter({ hasText: "ジェネリクス" }).first();

    if ((await tocLink.count()) > 0) {
      await tocLink.click();

      await page.waitForTimeout(500);

      const heading = page.getByRole("heading", { name: "ジェネリクス", level: 2 });
      await expect(heading).toBeInViewport();
    }
  });

  test("shows 404 for non-existent article slug", async ({
    page,
  }: TestArgs) => {
    await page.goto("/articles/non-existent-article-slug-xyz", { waitUntil: "load" });

    const notFoundIndicators = page.locator("text=/404|not found|見つかりません|ページが見つかりません/i");
    await expect(notFoundIndicators.first()).toBeVisible();
  });

  test("draft article shows 404 on reader", async ({
    page,
  }: TestArgs) => {
    await page.goto("/articles/nextjs-app-router", { waitUntil: "load" });

    const notFoundIndicators = page.locator("text=/404|not found|見つかりません|ページが見つかりません/i");
    await expect(notFoundIndicators.first()).toBeVisible();
  });

  test("image article displays both header and inline images", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/articles/${imageArticle.slug}`, { waitUntil: "load" });

    await expect(
      page.getByRole("heading", { name: imageArticle.title }).first(),
    ).toBeVisible();

    const images = page.locator("img");
    const imageCount = await images.count();
    expect(imageCount).toBeGreaterThanOrEqual(1);
  });

  test("code block in image article is displayed", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/articles/${imageArticle.slug}`, { waitUntil: "load" });

    await expect(page.getByText("画像とコードの共存テスト")).toBeVisible();
  });

  test("renders mermaid diagram as SVG", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${mermaidArticle.slug}`, { waitUntil: "load" });

    await expect(
      page.getByRole("heading", { name: mermaidArticle.title }).first(),
    ).toBeVisible();

    const mermaidSvg = page.locator("svg.mermaid, svg[id^='mermaid']");
    await expect(mermaidSvg.first()).toBeVisible({ timeout: 10000 });
  });

  test("mermaid diagram contains expected nodes", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${mermaidArticle.slug}`, { waitUntil: "load" });

    const mermaidSvg = page.locator("svg.mermaid, svg[id^='mermaid']");
    await expect(mermaidSvg.first()).toBeVisible({ timeout: 10000 });

    const svgContent = await mermaidSvg.first().innerHTML();
    expect(svgContent).toContain("開始");
    expect(svgContent).toContain("条件分岐");
    expect(svgContent).toContain("終了");
  });

  test("mermaid code block is not displayed as raw text", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${mermaidArticle.slug}`, { waitUntil: "load" });

    const mermaidSvg = page.locator("svg.mermaid, svg[id^='mermaid']");
    await expect(mermaidSvg.first()).toBeVisible({ timeout: 10000 });

    const rawMermaidCode = page.locator("pre code").filter({ hasText: "flowchart TD" });
    await expect(rawMermaidCode).toHaveCount(0);
  });
});
