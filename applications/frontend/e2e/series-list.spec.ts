import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const publishedSeriesNewestFirst = [
  {
    slug: "typescript-design-patterns",
    title: "実践TypeScript設計パターン",
  },
  {
    slug: "rust-system-programming",
    title: "Rustで学ぶシステムプログラミング",
  },
];

const draftSeries = {
  slug: "nextjs-fullstack-application",
  title: "Next.jsで作るフルスタックアプリケーション",
};

test.describe("series list page ordering", () => {
  test.describe("display order", () => {
    test("first series card in DOM is the newest series", async ({
      page,
    }: TestArgs) => {
      await page.goto("/series", { waitUntil: "load" });

      const seriesCardLinks = page
        .locator("main")
        .getByRole("link")
        .filter({ has: page.locator("article") });

      await expect(seriesCardLinks.first()).toBeVisible();

      const firstLinkHref = await seriesCardLinks
        .first()
        .getAttribute("href");
      expect(firstLinkHref).toContain(
        `/series/${publishedSeriesNewestFirst[0].slug}`,
      );

      const firstCardTitle = seriesCardLinks
        .first()
        .getByRole("heading", { level: 2 });
      await expect(firstCardTitle).toHaveText(
        publishedSeriesNewestFirst[0].title,
      );
    });

    test("series cards appear in newest first order in DOM", async ({
      page,
    }: TestArgs) => {
      await page.goto("/series", { waitUntil: "load" });

      const seriesCardLinks = page
        .locator("main")
        .getByRole("link")
        .filter({ has: page.locator("article") });

      await expect(seriesCardLinks.first()).toBeVisible();

      const hrefs = await seriesCardLinks.evaluateAll((elements) =>
        elements.map((element) => element.getAttribute("href") ?? ""),
      );

      const newestIndex = hrefs.findIndex((href) =>
        href.includes(`/series/${publishedSeriesNewestFirst[0].slug}`),
      );
      const secondIndex = hrefs.findIndex((href) =>
        href.includes(`/series/${publishedSeriesNewestFirst[1].slug}`),
      );

      expect(newestIndex).toBeGreaterThanOrEqual(0);
      expect(secondIndex).toBeGreaterThanOrEqual(0);
      expect(newestIndex).toBeLessThan(secondIndex);
    });

    test("newest series appears above older series visually", async ({
      page,
    }: TestArgs) => {
      await page.goto("/series", { waitUntil: "load" });

      const newestSeriesCard = page
        .locator("main")
        .getByRole("link")
        .filter({
          hasText: publishedSeriesNewestFirst[0].title,
        })
        .first();
      const olderSeriesCard = page
        .locator("main")
        .getByRole("link")
        .filter({
          hasText: publishedSeriesNewestFirst[1].title,
        })
        .first();

      await expect(newestSeriesCard).toBeVisible();
      await expect(olderSeriesCard).toBeVisible();

      const newestBox = await newestSeriesCard.boundingBox();
      const olderBox = await olderSeriesCard.boundingBox();

      expect(newestBox).not.toBeNull();
      expect(olderBox).not.toBeNull();
      expect(newestBox!.y).toBeLessThan(olderBox!.y);
    });
  });

  test.describe("published content", () => {
    test("displays all published series", async ({ page }: TestArgs) => {
      await page.goto("/series", { waitUntil: "load" });

      for (const series of publishedSeriesNewestFirst) {
        await expect(page.getByText(series.title).first()).toBeVisible();
      }
    });

    test("does not display draft series", async ({ page }: TestArgs) => {
      await page.goto("/series", { waitUntil: "load" });

      await expect(page.getByText(draftSeries.title)).not.toBeVisible();
    });
  });
});
