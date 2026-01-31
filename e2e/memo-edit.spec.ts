import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// シードデータの公開メモ
const publishedMemo = {
  slug: "go-tips",
  title: "Go言語のTips",
};

/**
 * Memo creation and editing tests
 */
test.describe("memo creation", () => {
  test("displays memo creation form", async ({ page }: TestArgs) => {
    await page.goto("/memos/new");

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Title input should be visible
    await expect(page.getByPlaceholder("Enter title...")).toBeVisible();

    // Create button should be visible
    await expect(
      page.getByRole("button", { name: "メモを作成" }),
    ).toBeVisible();
  });

  test("create button is disabled when title is empty", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new");

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Create button should be disabled
    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeDisabled();
  });

  test("create button is enabled when title is entered", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new");

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Enter title
    await page.getByPlaceholder("Enter title...").fill("テストメモ");

    // Create button should be enabled
    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeEnabled();
  });

  test("displays status dropdown with draft and published options", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new");

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // DropdownSelect is a custom component (button-based, not <select>)
    // Default value should be "下書き" (draft)
    const dropdownTrigger = page.getByRole("button", { name: "下書き" });
    await expect(dropdownTrigger).toBeVisible();

    // Click to open dropdown
    await dropdownTrigger.click();

    // Check options are displayed
    await expect(page.getByRole("button", { name: "公開" })).toBeVisible();
  });
});

test.describe("memo editing", () => {
  test("displays memo edit page with title", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Title should be displayed
    await expect(
      page.getByRole("heading", { name: publishedMemo.title }).first(),
    ).toBeVisible();
  });

  test("displays entry editor with Markdown tab", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Markdown tab should be visible and active by default
    await expect(page.getByRole("button", { name: "Markdown" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Preview" })).toBeVisible();
  });

  test("displays textarea for entry input", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Textarea should be visible with placeholder
    await expect(page.getByPlaceholder("コメントを追加")).toBeVisible();
  });

  test("displays submit button", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Submit button should be visible
    await expect(page.getByRole("button", { name: "投稿する" })).toBeVisible();
  });

  test("displays existing entries", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Existing entries should be displayed
    await expect(page.getByText("Go言語ではエラーは値として扱う")).toBeVisible();
  });
});

test.describe("memo entry preview", () => {
  test("switches to preview tab and shows empty message when no content", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Click Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Empty preview message should be shown
    await expect(
      page.getByText("プレビューするコンテンツがありません"),
    ).toBeVisible();
  });

  test("previews plain text content", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Enter plain text
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill("これはテストテキストです。");

    // Switch to Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Preview should show the text (react-markdown renders instantly)
    await expect(page.getByText("これはテストテキストです。")).toBeVisible();
  });

  test("previews inline code", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Enter text with inline code
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill("変数は `const value = 123` のように定義します。");

    // Switch to Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Preview should show inline code
    await expect(page.locator("code", { hasText: "const value = 123" })).toBeVisible();
  });

  test("previews code block", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Enter code block
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill("以下はTypeScriptのコード例です。\n\n```typescript\nfunction greet(name: string): string {\n  return `Hello, ${name}!`;\n}\n```");

    // Switch to Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Preview should show code block (react-syntax-highlighter renders instantly)
    await expect(page.locator("pre").first()).toBeVisible();
  });

  test("previews link", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Enter text with link
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill(
      "詳しくは[公式ドキュメント](https://example.com)を参照してください。",
    );

    // Switch to Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Preview should show link
    const link = page.getByRole("link", { name: "公式ドキュメント" });
    await expect(link).toBeVisible();
    await expect(link).toHaveAttribute("href", "https://example.com");
  });

  test("previews bold and italic text", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Enter text with bold and italic
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill("これは**太字**と*斜体*のテストです。");

    // Switch to Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Preview should show formatted text
    await expect(page.locator("strong", { hasText: "太字" })).toBeVisible();
    await expect(page.locator("em", { hasText: "斜体" })).toBeVisible();
  });

  test("previews list", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Enter unordered list
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill("買い物リスト：\n\n- りんご\n- バナナ\n- オレンジ");

    // Switch to Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Preview should show list items
    await expect(page.locator("li", { hasText: "りんご" })).toBeVisible();
    await expect(page.locator("li", { hasText: "バナナ" })).toBeVisible();
    await expect(page.locator("li", { hasText: "オレンジ" })).toBeVisible();
  });

  test("previews blockquote", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Enter blockquote
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill("> これは引用文です。\n> 複数行にわたることもあります。");

    // Switch to Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Preview should show blockquote
    await expect(page.locator("blockquote")).toBeVisible();
  });

  test("switches back to Markdown tab and preserves content", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    const testContent = "タブ切り替えテスト用のコンテンツ";

    // Enter content
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill(testContent);

    // Switch to Preview tab
    await page.getByRole("button", { name: "Preview" }).click();

    // Wait for preview
    await page.waitForTimeout(300);

    // Switch back to Markdown tab
    await page.getByRole("button", { name: "Markdown" }).click();

    // Content should be preserved
    await expect(textarea).toHaveValue(testContent);
  });
});

test.describe("memo entry submission", () => {
  test("can submit entry and see it in the list", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Generate unique content to identify this entry
    const uniqueContent = `E2Eテストエントリ ${Date.now()}`;

    // Enter content
    const textarea = page.getByPlaceholder("コメントを追加");
    await textarea.fill(uniqueContent);

    // Click submit button
    await page.getByRole("button", { name: "投稿する" }).click();

    // Wait for submission
    await page.waitForLoadState("networkidle");

    // Textarea should be cleared after successful submission
    await expect(textarea).toHaveValue("");

    // New entry should appear in the list
    await expect(page.getByText(uniqueContent)).toBeVisible();
  });
});

test.describe("memo preview page", () => {
  test("displays entry content on memo detail page", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Title should be displayed
    await expect(
      page.getByRole("heading", { name: publishedMemo.title }).first(),
    ).toBeVisible();

    // Entry content should be displayed
    await expect(page.getByText("Go言語ではエラーは値として扱う")).toBeVisible();
  });

  test("renders inline code on memo detail page", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Inline code should be rendered
    await expect(page.locator("code", { hasText: "error" })).toBeVisible();
  });
});
