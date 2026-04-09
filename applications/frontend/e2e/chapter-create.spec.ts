import { expect, type Page, test } from "@playwright/test";
import {
  getChapterIdentifierBySlug,
  getContentTokenIndex,
} from "./helpers/search-token";

type TestArgs = {
  page: Page;
};

const seriesSlug = "rust-system-programming";
const existingChapterSlug = "chapter-1-rust-basics";

test.describe("chapter create page", () => {
  test("page is accessible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/new`, { waitUntil: "load" });

    await expect(page.locator("main")).toBeVisible({ timeout: 15000 });
  });

  test("CodeMirror editor is visible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/new`, { waitUntil: "load" });

    await expect(
      page.locator(".cm-content"),
    ).toBeVisible({ timeout: 15000 });
  });

  test("save button is present", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/new`, { waitUntil: "load" });

    await expect(
      page.getByRole("button", { name: /保存|下書き保存|公開する/ }),
    ).toBeVisible({ timeout: 15000 });
  });
});

test.describe("chapter search token verification", () => {
  test("chapter identifier exists in Firestore", async () => {
    const chapterIdentifier =
      await getChapterIdentifierBySlug(existingChapterSlug);

    expect(chapterIdentifier).toBeDefined();
  });

  test("search tokens exist for published chapter", async () => {
    const chapterIdentifier =
      await getChapterIdentifierBySlug(existingChapterSlug);

    if (chapterIdentifier === undefined) {
      throw new Error(
        `Chapter identifier not found for slug: ${existingChapterSlug}`,
      );
    }

    const tokenIndex = await getContentTokenIndex(
      "chapter",
      chapterIdentifier,
    );

    expect(tokenIndex).toBeDefined();

    if (tokenIndex === undefined) {
      return;
    }

    expect(tokenIndex.tokens.length).toBeGreaterThan(0);
  });
});
