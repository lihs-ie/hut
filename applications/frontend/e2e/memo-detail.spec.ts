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
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    await expect(
      page.getByRole("heading", { name: publishedMemo.title, level: 1 }).first(),
    ).toBeVisible();
  });

  test("displays publication date", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    const publishedDateText = page.locator("text=投稿日時：");
    await expect(publishedDateText).toBeVisible();

    const datePattern = /\d{4}\/\d{2}\/\d{2}/;
    const mainContent = await page.locator("main").textContent();
    expect(mainContent).toMatch(datePattern);
  });

  test("displays last updated date", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    const updatedDateText = page.locator("text=最終更新日時：");
    await expect(updatedDateText).toBeVisible();
  });

  test("displays tag with link", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    const tagLink = page.getByRole("link", { name: publishedMemo.tags[0] });
    await expect(tagLink).toBeVisible();

    const href = await tagLink.getAttribute("href");
    expect(href).toBeTruthy();
  });

  test("displays multiple entries", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    await expect(page.getByText("Go言語ではエラーは値として扱う")).toBeVisible();

    await expect(page.getByText("goroutineは軽量スレッド")).toBeVisible();
  });

  test("displays entry creation timestamps", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    const timePattern = /\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}/;
    const mainContent = await page.locator("main").textContent();

    const matches = mainContent?.match(new RegExp(timePattern, "g"));
    expect(matches?.length).toBeGreaterThanOrEqual(2);
  });

  test("renders inline code in entry text", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    const inlineCode = page.locator("code");
    const codeCount = await inlineCode.count();
    expect(codeCount).toBeGreaterThan(0);

    await expect(page.locator("code", { hasText: "error" })).toBeVisible();
  });

  test("memo entries are displayed in sections", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    const sections = page.locator("article section");
    const sectionCount = await sections.count();

    expect(sectionCount).toBeGreaterThanOrEqual(2);
  });

  test("shows 404 for non-existent memo slug", async ({ page }: TestArgs) => {
    await page.goto("/memos/non-existent-memo-slug-xyz", { waitUntil: "load" });

    const notFoundIndicators = page.locator(
      "text=/404|not found|見つかりません|ページが見つかりません/i",
    );
    await expect(notFoundIndicators.first()).toBeVisible();
  });

  test("draft memo shows 404 on reader", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${draftMemo.slug}`, { waitUntil: "load" });

    const notFoundIndicators = page.locator("text=/404|not found|見つかりません|ページが見つかりません/i");
    await expect(notFoundIndicators.first()).toBeVisible();
  });

  test("draft memo with three entries shows 404 on reader", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${draftMemo.slug}`, { waitUntil: "load" });

    const notFoundIndicators = page.locator("text=/404|not found|見つかりません|ページが見つかりません/i");
    await expect(notFoundIndicators.first()).toBeVisible();
  });

  test("entry text is rendered as prose", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    await expect(
      page.getByText("Go言語ではエラーは値として扱う"),
    ).toBeVisible();
  });

  test("clock icon is displayed with entry timestamps", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`, { waitUntil: "load" });

    const clockIcons = page.locator("section i span[role='img']");
    const iconCount = await clockIcons.count();

    expect(iconCount).toBeGreaterThanOrEqual(2);
  });
});
