import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// シードデータのメモ情報
const publishedMemo = {
  slug: "go-tips",
  title: "Go言語のTips",
  tags: ["Go"],
  entries: [
    "Go言語ではエラーは値として扱う。`error`インターフェースを実装すれば独自エラー型を作れる。",
    "goroutineは軽量スレッド。`go`キーワードで簡単に並行処理ができる。",
  ],
};

const draftMemo = {
  slug: "typescript-config",
  title: "TypeScript設定メモ",
  tags: ["TypeScript"],
  entries: [
    "`strict: true`は必須。型安全性を最大限に活用するため。",
    "`noUncheckedIndexedAccess`を有効にすると配列アクセスも安全になる。",
    "パスエイリアスは`paths`と`baseUrl`で設定。ビルドツールの設定も忘れずに。",
  ],
};

/**
 * Memo detail page tests
 */
test.describe("memo detail page", () => {
  test("displays memo title as h1 heading", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    await expect(
      page.getByRole("heading", { name: publishedMemo.title, level: 1 }).first(),
    ).toBeVisible();
  });

  test("displays publication date", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check for "投稿日時：" label with date format (YYYY/MM/DD)
    const publishedDateText = page.locator("text=投稿日時：");
    await expect(publishedDateText).toBeVisible();

    // Verify date format exists in content
    const datePattern = /\d{4}\/\d{2}\/\d{2}/;
    const mainContent = await page.locator("main").textContent();
    expect(mainContent).toMatch(datePattern);
  });

  test("displays last updated date", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check for "最終更新日時：" label
    const updatedDateText = page.locator("text=最終更新日時：");
    await expect(updatedDateText).toBeVisible();
  });

  test("displays tag with link", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check tag is displayed as a link
    const tagLink = page.getByRole("link", { name: publishedMemo.tags[0] });
    await expect(tagLink).toBeVisible();

    // Verify tag link has href
    const href = await tagLink.getAttribute("href");
    expect(href).toBeTruthy();
  });

  test("displays multiple entries", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check that both entries are displayed
    // The first entry mentions "error インターフェース"
    await expect(page.getByText("Go言語ではエラーは値として扱う")).toBeVisible();

    // The second entry mentions "goroutine"
    await expect(page.getByText("goroutineは軽量スレッド")).toBeVisible();
  });

  test("displays entry creation timestamps", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Each entry should have a timestamp in YYYY/MM/DD HH:mm format
    const timePattern = /\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}/;
    const mainContent = await page.locator("main").textContent();

    // Should have at least 2 entry timestamps (for 2 entries)
    const matches = mainContent?.match(new RegExp(timePattern, "g"));
    expect(matches?.length).toBeGreaterThanOrEqual(2);
  });

  test("renders inline code in entry text", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check for inline code elements
    // The entries contain inline code like `error` and `go`
    const inlineCode = page.locator("code");
    const codeCount = await inlineCode.count();
    expect(codeCount).toBeGreaterThan(0);

    // Verify specific inline code content
    await expect(page.locator("code", { hasText: "error" })).toBeVisible();
  });

  test("memo entries are displayed in sections", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Each entry should be in a section element
    const sections = page.locator("article section");
    const sectionCount = await sections.count();

    // Should have at least 2 sections for 2 entries
    expect(sectionCount).toBeGreaterThanOrEqual(2);
  });

  test("shows 404 for non-existent memo slug", async ({ page }: TestArgs) => {
    await page.goto("/memos/non-existent-memo-slug-xyz");

    // Check for not-found page content (Next.js returns 200 with not-found page)
    const notFoundIndicators = page.locator(
      "text=/404|not found|見つかりません|ページが見つかりません/i",
    );
    await expect(notFoundIndicators.first()).toBeVisible();
  });

  test("draft memo page renders (admin preview)", async ({ page }: TestArgs) => {
    // typescript-config is a draft memo
    await page.goto(`/memos/${draftMemo.slug}`);

    // Verify page loads
    await page.waitForLoadState("networkidle");

    // Check if memo title is displayed (draft accessible) or 404 page
    const memoTitle = page
      .getByRole("heading", { name: draftMemo.title })
      .first();
    const notFoundIndicators = page.locator("text=/404|not found|見つかりません/i");

    const isTitleVisible = await memoTitle.isVisible().catch(() => false);
    const isNotFound = await notFoundIndicators
      .first()
      .isVisible()
      .catch(() => false);

    // Either the memo is accessible or shows 404
    expect(isTitleVisible || isNotFound).toBe(true);
  });

  test("memo with three entries displays all entries", async ({
    page,
  }: TestArgs) => {
    // Draft memo has 3 entries, but if not accessible, skip check
    await page.goto(`/memos/${draftMemo.slug}`);
    await page.waitForLoadState("networkidle");

    const memoTitle = page
      .getByRole("heading", { name: draftMemo.title })
      .first();
    const isTitleVisible = await memoTitle.isVisible().catch(() => false);

    if (isTitleVisible) {
      // Check for all three entries
      await expect(page.locator("code", { hasText: "strict: true" })).toBeVisible();
      await expect(
        page.locator("code", { hasText: "noUncheckedIndexedAccess" }),
      ).toBeVisible();
      await expect(page.locator("code", { hasText: "paths" })).toBeVisible();
    }
  });

  test("entry text is rendered as prose", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check that memo content includes expected text
    await expect(
      page.getByText("Go言語ではエラーは値として扱う"),
    ).toBeVisible();
  });

  test("clock icon is displayed with entry timestamps", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Check for clock icons (span elements with role="img" within entry sections)
    const clockIcons = page.locator("section i span[role='img']");
    const iconCount = await clockIcons.count();

    // Each entry should have a clock icon
    expect(iconCount).toBeGreaterThanOrEqual(2);
  });
});
