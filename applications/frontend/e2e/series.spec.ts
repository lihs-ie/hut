import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const publishedSeries = {
  slug: "rust-system-programming",
  title: "Rustで学ぶシステムプログラミング",
  chapters: [
    {
      slug: "chapter-1-rust-basics",
      title: "第1章: Rustの基礎",
    },
    {
      slug: "chapter-2-memory-management",
      title: "第2章: メモリ管理",
    },
  ],
};

test.describe("series list page", () => {
  test("series list page renders", async ({ page }: TestArgs) => {
    await page.goto("/series");

    await expect(page.locator("main")).toBeVisible();
  });
});

test.describe("series detail page", () => {
  test("displays series title", async ({ page }: TestArgs) => {
    await page.goto(`/series/${publishedSeries.slug}`);

    await page.waitForLoadState("networkidle");

    await expect(
      page.getByText(publishedSeries.title).first(),
    ).toBeVisible();
  });

  test("displays chapter list", async ({ page }: TestArgs) => {
    await page.goto(`/series/${publishedSeries.slug}`);

    await page.waitForLoadState("networkidle");

    for (const chapter of publishedSeries.chapters) {
      await expect(
        page.getByText(chapter.title).first(),
      ).toBeVisible();
    }
  });

  test("shows 404 for non-existent series slug", async ({
    page,
  }: TestArgs) => {
    await page.goto("/series/non-existent-series-slug-xyz");

    const notFoundIndicators = page.locator(
      "text=/404|not found|見つかりません|ページが見つかりません/i",
    );
    await expect(notFoundIndicators.first()).toBeVisible();
  });
});

test.describe("series chapter page", () => {
  test("displays chapter title as h1 heading", async ({
    page,
  }: TestArgs) => {
    const chapter = publishedSeries.chapters[0];
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/${chapter.slug}`,
    );

    await page.waitForLoadState("networkidle");

    await expect(
      page
        .getByRole("heading", { name: chapter.title, level: 1 })
        .first(),
    ).toBeVisible();
  });

  test("displays chapter label with chapter number", async ({
    page,
  }: TestArgs) => {
    const chapter = publishedSeries.chapters[0];
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/${chapter.slug}`,
    );

    await page.waitForLoadState("networkidle");

    await expect(page.getByText("Chapter 1")).toBeVisible();
  });

  test("displays series title as a link back to series detail", async ({
    page,
  }: TestArgs) => {
    const chapter = publishedSeries.chapters[0];
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/${chapter.slug}`,
    );

    await page.waitForLoadState("networkidle");

    const seriesLink = page.getByRole("link", {
      name: publishedSeries.title,
    });
    await expect(seriesLink).toBeVisible();

    const href = await seriesLink.getAttribute("href");
    expect(href).toBe(`/series/${publishedSeries.slug}`);
  });

  test("displays chapter navigation sidebar", async ({
    page,
  }: TestArgs) => {
    const chapter = publishedSeries.chapters[0];
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/${chapter.slug}`,
    );

    await page.waitForLoadState("networkidle");

    for (const ch of publishedSeries.chapters) {
      await expect(page.getByText(ch.title).first()).toBeVisible();
    }
  });

  test("displays next chapter navigation on first chapter", async ({
    page,
  }: TestArgs) => {
    const chapter = publishedSeries.chapters[0];
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/${chapter.slug}`,
    );

    await page.waitForLoadState("networkidle");

    await expect(page.getByText("次の章")).toBeVisible();
  });

  test("displays previous chapter navigation on second chapter", async ({
    page,
  }: TestArgs) => {
    const chapter = publishedSeries.chapters[1];
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/${chapter.slug}`,
    );

    await page.waitForLoadState("networkidle");

    await expect(page.getByText("前の章")).toBeVisible();
  });

  test("displays article content as prose", async ({
    page,
  }: TestArgs) => {
    const chapter = publishedSeries.chapters[0];
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/${chapter.slug}`,
    );

    await page.waitForLoadState("networkidle");

    await expect(
      page.getByText("Rustの基本的な文法と特徴について学びます"),
    ).toBeVisible();
  });

  test("displays code blocks with syntax highlighting", async ({
    page,
  }: TestArgs) => {
    const chapter = publishedSeries.chapters[0];
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/${chapter.slug}`,
    );

    await page.waitForLoadState("networkidle");

    const codeBlocks = page.locator("pre code, .shiki");
    await expect(codeBlocks.first()).toBeVisible();
  });

  test("shows 404 for non-existent chapter slug", async ({
    page,
  }: TestArgs) => {
    await page.goto(
      `/series/${publishedSeries.slug}/chapters/non-existent-chapter`,
    );

    const notFoundIndicators = page.locator(
      "text=/404|not found|見つかりません|チャプターが見つかりません/i",
    );
    await expect(notFoundIndicators.first()).toBeVisible();
  });
});
