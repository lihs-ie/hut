import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const publishedMemo = {
  slug: "go-tips",
  title: "Go言語のTips",
};

const fillCodeMirrorEditor = async (page: Page, content: string): Promise<void> => {
  const editorContent = page.locator(".cm-content");
  await editorContent.click();
  await page.keyboard.press("Control+a");
  await page.keyboard.type(content);
};

test.describe("memo creation", () => {
  test("displays memo creation form", async ({ page }: TestArgs) => {
    await page.goto("/memos/new");

    await page.waitForLoadState("networkidle");

    await expect(page.getByPlaceholder("Enter title...")).toBeVisible();

    await expect(
      page.getByRole("button", { name: "メモを作成" }),
    ).toBeVisible();
  });

  test("create button is disabled when title is empty", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new");

    await page.waitForLoadState("networkidle");

    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeDisabled();
  });

  test("create button is enabled when title is entered", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new");

    await page.waitForLoadState("networkidle");

    await page.getByPlaceholder("Enter title...").fill("テストメモ");

    const createButton = page.getByRole("button", { name: "メモを作成" });
    await expect(createButton).toBeEnabled();
  });

  test("displays status dropdown with draft and published options", async ({
    page,
  }: TestArgs) => {
    await page.goto("/memos/new");

    await page.waitForLoadState("networkidle");

    const dropdownTrigger = page.getByRole("button", { name: "下書き" });
    await expect(dropdownTrigger).toBeVisible();

    await dropdownTrigger.click();

    await expect(page.getByRole("button", { name: "公開" })).toBeVisible();
  });
});

test.describe("memo editing", () => {
  test("displays memo edit page with title", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await expect(
      page.getByRole("heading", { name: publishedMemo.title }).first(),
    ).toBeVisible();
  });

  test("displays entry editor with Markdown tab", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await expect(page.getByRole("button", { name: "Markdown" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Preview" })).toBeVisible();
  });

  test("displays codemirror editor for entry input", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await expect(page.locator(".cm-editor")).toBeVisible();
  });

  test("displays submit button", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await expect(page.getByRole("button", { name: "投稿する" })).toBeVisible();
  });

  test("displays existing entries", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await expect(page.getByText("Go言語ではエラーは値として扱う")).toBeVisible();
  });
});

test.describe("memo entry preview", () => {
  test("switches to preview tab and shows empty message when no content", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await page.getByRole("button", { name: "Preview" }).click();

    await expect(
      page.getByText("プレビューするコンテンツがありません"),
    ).toBeVisible();
  });

  test("previews plain text content", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await fillCodeMirrorEditor(page, "これはテストテキストです。");

    await page.getByRole("button", { name: "Preview" }).click();

    await expect(page.getByText("これはテストテキストです。")).toBeVisible();
  });

  test("previews inline code", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await fillCodeMirrorEditor(page, "変数は `const value = 123` のように定義します。");

    await page.getByRole("button", { name: "Preview" }).click();

    await expect(page.locator("code", { hasText: "const value = 123" })).toBeVisible();
  });

  test("previews code block", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await fillCodeMirrorEditor(page, "以下はTypeScriptのコード例です。\n\n```typescript\nfunction greet(name: string): string {\n  return `Hello, ${name}!`;\n}\n```");

    await page.getByRole("button", { name: "Preview" }).click();

    await expect(page.locator("pre").first()).toBeVisible();
  });

  test("previews link", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await fillCodeMirrorEditor(
      page,
      "詳しくは[公式ドキュメント](https://example.com)を参照してください。",
    );

    await page.getByRole("button", { name: "Preview" }).click();

    const link = page.getByRole("link", { name: "公式ドキュメント" });
    await expect(link).toBeVisible();
    await expect(link).toHaveAttribute("href", "https://example.com");
  });

  test("previews bold and italic text", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await fillCodeMirrorEditor(page, "これは**太字**と*斜体*のテストです。");

    await page.getByRole("button", { name: "Preview" }).click();

    await expect(page.locator("strong", { hasText: "太字" })).toBeVisible();
    await expect(page.locator("em", { hasText: "斜体" })).toBeVisible();
  });

  test("previews list", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await fillCodeMirrorEditor(page, "買い物リスト：\n\n- りんご\n- バナナ\n- オレンジ");

    await page.getByRole("button", { name: "Preview" }).click();

    await expect(page.locator("li", { hasText: "りんご" }).first()).toBeVisible();
    await expect(page.locator("li").filter({ hasText: /^バナナ$/ })).toBeVisible();
    await expect(page.locator("li").filter({ hasText: /^オレンジ$/ })).toBeVisible();
  });

  test("previews blockquote", async ({ page }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    await fillCodeMirrorEditor(page, "> これは引用文です。\n> 複数行にわたることもあります。");

    await page.getByRole("button", { name: "Preview" }).click();

    await expect(page.locator("blockquote").first()).toBeVisible();
  });

  test("switches back to Markdown tab and preserves content", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    const testContent = "タブ切り替えテスト用のコンテンツ";

    await fillCodeMirrorEditor(page, testContent);

    await page.getByRole("button", { name: "Preview" }).click();

    await expect(page.getByText(testContent)).toBeVisible();

    await page.getByRole("button", { name: "Markdown" }).click();

    const editorContent = page.locator(".cm-content");
    await expect(editorContent).toContainText(testContent);
  });
});

test.describe("memo entry submission", () => {
  test("can submit entry and see it in the list", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}/edit`);

    await page.waitForLoadState("networkidle");

    const uniqueContent = `E2Eテストエントリ ${Date.now()}`;

    await fillCodeMirrorEditor(page, uniqueContent);

    await page.getByRole("button", { name: "投稿する" }).click();

    await page.waitForLoadState("networkidle");

    await expect(page.getByText(uniqueContent)).toBeVisible({ timeout: 30000 });
  });
});

test.describe("memo preview page", () => {
  test("displays entry content on memo detail page", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    await page.waitForLoadState("networkidle");

    await expect(
      page.getByRole("heading", { name: publishedMemo.title }).first(),
    ).toBeVisible();

    await expect(page.getByText("Go言語ではエラーは値として扱う")).toBeVisible();
  });

  test("renders inline code on memo detail page", async ({
    page,
  }: TestArgs) => {
    await page.goto(`/memos/${publishedMemo.slug}`);

    await page.waitForLoadState("networkidle");

    await expect(page.locator("code", { hasText: "error" })).toBeVisible();
  });
});
