import { expect, type Page, test } from "@playwright/test";
import {
  getChapterIdentifierBySlug,
  waitForSearchTokens,
} from "./helpers/search-token";

type TestArgs = {
  page: Page;
};

const seriesSlug = "rust-system-programming";
const chapterSlug = "chapter-1-rust-basics";

test.describe("chapter edit page", () => {
  test("page is accessible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`);

    await expect(page.locator("main")).toBeVisible({ timeout: 15000 });
  });

  test("CodeMirror editor is visible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`);

    await expect(
      page.locator(".cm-content"),
    ).toBeVisible({ timeout: 15000 });
  });

  test("save button is present", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`);

    await expect(
      page.getByRole("button", { name: /保存|下書き保存|公開する/ }),
    ).toBeVisible({ timeout: 15000 });
  });
});

test.describe("chapter search token verification", () => {
  test("search tokens exist for published chapter", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`);
    await page.waitForLoadState("networkidle");

    const chapterIdentifier = await getChapterIdentifierBySlug(chapterSlug);

    if (chapterIdentifier === undefined) {
      test.skip(true, "chapter identifier not found in Firestore");
      return;
    }

    const tokenIndex = await waitForSearchTokens(
      "chapter",
      chapterIdentifier,
      30000,
    );

    expect(tokenIndex).toBeDefined();

    if (tokenIndex === undefined) {
      return;
    }

    expect(tokenIndex.tokens.length).toBeGreaterThan(0);
  });
});
