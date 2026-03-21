import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const series = {
  slug: "rust-system-programming",
  title: "Rustで学ぶシステムプログラミング",
  subTitle: "低レイヤーからRustを理解する",
  description:
    "Rustを使ってシステムプログラミングの基礎を学ぶシリーズです。メモリ管理、並行処理、ネットワークプログラミングなどを扱います。",
  tags: ["Rust"],
  chapters: [
    {
      slug: "chapter-1-rust-basics",
      title: "第1章: Rustの基礎",
      number: 1,
    },
    {
      slug: "chapter-2-memory-management",
      title: "第2章: メモリ管理",
      number: 2,
    },
  ],
};

test.describe("series list page", () => {
  test.describe("page structure", () => {
    test("page loads successfully", async ({ page }: TestArgs) => {
      await page.goto("/series");
      await page.waitForLoadState("networkidle");

      await expect(page.locator("main")).toBeVisible();
    });

    test("displays series list heading", async ({ page }: TestArgs) => {
      await page.goto("/series");
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: "シリーズ" }),
      ).toBeVisible();
    });

    test("displays description text", async ({ page }: TestArgs) => {
      await page.goto("/series");
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByText("技術書や体系的にまとめられたコンテンツ"),
      ).toBeVisible();
    });
  });

  test.describe("series card", () => {
    test("displays seeded series title", async ({ page }: TestArgs) => {
      await page.goto("/series");
      await page.waitForLoadState("networkidle");

      await expect(page.getByText(series.title).first()).toBeVisible();
    });

    test("series card links to detail page", async ({ page }: TestArgs) => {
      await page.goto("/series");
      await page.waitForLoadState("networkidle");

      const link = page
        .getByRole("link")
        .filter({ hasText: series.title })
        .first();
      const href = await link.getAttribute("href");
      expect(href).toContain(`/series/${series.slug}`);
    });

    test("clicking series card navigates to detail page", async ({
      page,
    }: TestArgs) => {
      await page.goto("/series");
      await page.waitForLoadState("networkidle");

      const link = page
        .getByRole("link")
        .filter({ hasText: series.title })
        .first();
      await link.click();

      await expect(page).toHaveURL(new RegExp(`/series/${series.slug}`));
    });
  });

  test.describe("responsiveness", () => {
    test("page is viewable on mobile viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 375, height: 667 });
      await page.goto("/series");
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: "シリーズ" }),
      ).toBeVisible();
    });

    test("page is viewable on tablet viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 768, height: 1024 });
      await page.goto("/series");
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: "シリーズ" }),
      ).toBeVisible();
    });
  });
});

test.describe("series detail page 404", () => {
  test("shows 404 for non-existent series slug", async ({
    page,
  }: TestArgs) => {
    await page.goto("/series/non-existent-slug");
    await page.waitForLoadState("networkidle");

    const notFoundIndicators = page.locator(
      "text=/404|not found|見つかりません|ページが見つかりません/i",
    );
    await expect(notFoundIndicators.first()).toBeVisible();
  });

  test("shows 404 for non-existent chapter slug", async ({
    page,
  }: TestArgs) => {
    await page.goto(
      `/series/${series.slug}/chapters/non-existent`,
    );
    await page.waitForLoadState("networkidle");

    const notFoundIndicators = page.locator(
      "text=/404|not found|見つかりません|ページが見つかりません/i",
    );
    await expect(notFoundIndicators.first()).toBeVisible();
  });
});

test.describe("series detail page", () => {
  test.describe("page structure", () => {
    test("page loads successfully", async ({ page }: TestArgs) => {
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      await expect(page.locator("main")).toBeVisible();
    });

    test("displays series title as h1 heading", async ({
      page,
    }: TestArgs) => {
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: series.title, level: 1 }),
      ).toBeVisible();
    });

    test("displays series subtitle", async ({ page }: TestArgs) => {
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      await expect(page.getByText(series.subTitle).first()).toBeVisible();
    });

    test("displays series description", async ({ page }: TestArgs) => {
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      await expect(page.getByText(series.description).first()).toBeVisible();
    });
  });

  test.describe("table of contents", () => {
    test("displays 目次 heading", async ({ page }: TestArgs) => {
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: "目次" }),
      ).toBeVisible();
    });

    test("displays all chapter titles in table of contents", async ({
      page,
    }: TestArgs) => {
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      for (const chapter of series.chapters) {
        await expect(
          page.getByRole("link").filter({ hasText: chapter.title }).first(),
        ).toBeVisible();
      }
    });

    test("chapter links point to correct URLs", async ({
      page,
    }: TestArgs) => {
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      for (const chapter of series.chapters) {
        const link = page
          .getByRole("link")
          .filter({ hasText: chapter.title })
          .first();
        const href = await link.getAttribute("href");
        expect(href).toContain(
          `/series/${series.slug}/chapters/${chapter.slug}`,
        );
      }
    });
  });

  test.describe("tags", () => {
    test("displays series tags", async ({ page }: TestArgs) => {
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      for (const tag of series.tags) {
        await expect(page.getByText(tag).first()).toBeVisible();
      }
    });
  });

  test.describe("responsiveness", () => {
    test("page is viewable on mobile viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 375, height: 667 });
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: series.title, level: 1 }),
      ).toBeVisible();
    });

    test("page is viewable on tablet viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 768, height: 1024 });
      await page.goto(`/series/${series.slug}`);
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByRole("heading", { name: series.title, level: 1 }),
      ).toBeVisible();
    });
  });
});

test.describe("chapter page", () => {
  const firstChapter = series.chapters[0];
  const secondChapter = series.chapters[1];

  test.describe("page structure", () => {
    test("page loads successfully", async ({ page }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      await expect(page.locator("article")).toBeVisible();
    });

    test("displays chapter title as h1 heading", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      await expect(
        page
          .getByRole("heading", { name: firstChapter.title, level: 1 })
          .first(),
      ).toBeVisible();
    });

    test("displays chapter number label", async ({ page }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      await expect(
        page.getByText(`Chapter ${firstChapter.number}`).first(),
      ).toBeVisible();
    });

    test("displays chapter markdown content", async ({ page }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      await expect(page.locator(".prose")).toBeVisible();
      await expect(
        page.getByRole("heading", { name: "変数と不変性", level: 2 }),
      ).toBeVisible();
    });

    test("displays code blocks in chapter content", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const codeBlocks = page.locator("pre code, .shiki");
      await expect(codeBlocks.first()).toBeVisible();
    });
  });

  test.describe("sidebar navigation", () => {
    test("displays series title link in sidebar", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const seriesLink = page.getByRole("link", { name: series.title });
      await expect(seriesLink.first()).toBeVisible();
    });

    test("series title link points to series detail page", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const seriesLink = page.getByRole("link", { name: series.title }).first();
      const href = await seriesLink.getAttribute("href");
      expect(href).toContain(`/series/${series.slug}`);
    });

    test("displays all chapters in sidebar navigation", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      for (const chapter of series.chapters) {
        await expect(page.getByText(chapter.title).first()).toBeVisible();
      }
    });

    test("current chapter is highlighted as active in sidebar", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const activeItem = page.locator('[class*="nav-item-active"]');
      await expect(activeItem).toBeVisible();

      const activeText = await activeItem.textContent();
      expect(activeText).toContain(firstChapter.title);
    });
  });

  test.describe("chapter navigation", () => {
    test("first chapter has no 前の章 button", async ({ page }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const prevButton = page.getByRole("link", { name: /前の章/ });
      await expect(prevButton).not.toBeVisible();
    });

    test("first chapter has 次の章 button linking to second chapter", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const nextButton = page.getByRole("link", { name: /次の章/ });
      await expect(nextButton).toBeVisible();

      const href = await nextButton.getAttribute("href");
      expect(href).toContain(
        `/series/${series.slug}/chapters/${secondChapter.slug}`,
      );
    });

    test("second chapter has 前の章 button linking to first chapter", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${secondChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const prevButton = page.getByRole("link", { name: /前の章/ });
      await expect(prevButton).toBeVisible();

      const href = await prevButton.getAttribute("href");
      expect(href).toContain(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
    });

    test("last chapter has no 次の章 button", async ({ page }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${secondChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const nextButton = page.getByRole("link", { name: /次の章/ });
      await expect(nextButton).not.toBeVisible();
    });

    test("clicking 次の章 navigates to next chapter", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const nextButton = page.getByRole("link", { name: /次の章/ });
      await nextButton.click();

      await expect(page).toHaveURL(
        new RegExp(
          `/series/${series.slug}/chapters/${secondChapter.slug}`,
        ),
      );
    });

    test("clicking 前の章 navigates to previous chapter", async ({
      page,
    }: TestArgs) => {
      await page.goto(
        `/series/${series.slug}/chapters/${secondChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      const prevButton = page.getByRole("link", { name: /前の章/ });
      await prevButton.click();

      await expect(page).toHaveURL(
        new RegExp(
          `/series/${series.slug}/chapters/${firstChapter.slug}`,
        ),
      );
    });
  });

  test.describe("responsiveness", () => {
    test("page is viewable on mobile viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 375, height: 667 });
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      await expect(
        page
          .getByRole("heading", { name: firstChapter.title, level: 1 })
          .first(),
      ).toBeVisible();
    });

    test("page is viewable on tablet viewport", async ({ page }: TestArgs) => {
      await page.setViewportSize({ width: 768, height: 1024 });
      await page.goto(
        `/series/${series.slug}/chapters/${firstChapter.slug}`,
      );
      await page.waitForLoadState("networkidle");

      await expect(
        page
          .getByRole("heading", { name: firstChapter.title, level: 1 })
          .first(),
      ).toBeVisible();
    });
  });
});
