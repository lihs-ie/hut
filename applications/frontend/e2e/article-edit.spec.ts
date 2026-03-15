import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const publishedArticleSlug = "typescript-type-safe-code";
const publishedArticleTitle = "TypeScriptで型安全なコードを書く";
const draftArticleSlug = "nextjs-app-router";
const draftArticleTitle = "Next.js App Routerの使い方";
const multiTagArticleSlug = "react-component-patterns";
const multiTagArticleTitle = "Reactコンポーネント設計パターン";

test.describe.serial("article edit tests", () => {
  /**
   * Test: Existing article data is displayed correctly.
   */
  test("existing article data is displayed", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`);

    const titleInput = page.getByPlaceholder("タイトルを入力");
    await expect(titleInput).toHaveValue(publishedArticleTitle);

    const editorContent = page.locator(".cm-content");
    await expect(editorContent).toContainText("TypeScriptで型安全なコードを書く");
    await expect(editorContent).toContainText("typescript-type-safe-code");

    await expect(page.getByRole("button", { name: "TypeScriptを削除" })).toBeVisible();
  });

  /**
   * Test: Draft article shows correct publish switch state.
   */
  test("draft article shows correct state", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${draftArticleSlug}/edit`);

    const titleInput = page.getByPlaceholder("タイトルを入力");
    await expect(titleInput).toHaveValue(draftArticleTitle);

    const publishSwitch = page.getByRole("checkbox");
    await expect(publishSwitch).not.toBeChecked();

    await expect(page.getByRole("button", { name: /下書き保存/ })).toBeVisible();
  });

  /**
   * Test: Remove a tag from article (non-destructive - just verifies UI).
   */
  test("remove tag from article", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${multiTagArticleSlug}/edit`);

    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(multiTagArticleTitle);

    const reactRemoveButton = page.getByRole("button", { name: "Reactを削除" });
    await expect(reactRemoveButton).toBeVisible();

    await reactRemoveButton.click();

    await expect(reactRemoveButton).not.toBeVisible();

    await expect(page.getByRole("button", { name: "React" })).toBeVisible();
  });

  /**
   * Test: Edit article content using CodeMirror editor.
   */
  test("edit article content", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`);

    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(publishedArticleTitle);

    const editorContent = page.locator(".cm-content");
    await editorContent.click();
    await page.keyboard.press("Control+End");
    await page.keyboard.type("\n\n## E2Eテストで追加したセクション\n\nこれはテストです。");

    await expect(editorContent).toContainText("E2Eテストで追加したセクション");

    await expect(page.getByRole("button", { name: /下書き保存|公開する/ })).toBeEnabled();
  });

  /**
   * Test: Toggle publish switch changes button text.
   */
  test("toggle publish switch changes button", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${draftArticleSlug}/edit`);

    const publishSwitch = page.getByRole("checkbox");
    await expect(publishSwitch).not.toBeChecked();
    await expect(page.getByRole("button", { name: /下書き保存/ })).toBeVisible();

    await page.getByPlaceholder("タイトルを入力").scrollIntoViewIfNeeded();
    await publishSwitch.evaluate((element: HTMLInputElement) => {
      element.click();
    });

    await expect(publishSwitch).toBeChecked();

    await expect(page.getByRole("button", { name: /公開する/ })).toBeVisible();

    await publishSwitch.evaluate((element: HTMLInputElement) => {
      element.click();
    });

    await expect(page.getByRole("button", { name: /下書き保存/ })).toBeVisible();
  });

  /**
   * Test: Editor toolbar buttons are visible.
   */
  test("editor toolbar is visible", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`);

    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(publishedArticleTitle);

    await expect(page.getByRole("button", { name: /太字/ })).toBeVisible();
    await expect(page.getByRole("button", { name: /斜体/ })).toBeVisible();
    await expect(page.getByRole("button", { name: /リンク/ })).toBeVisible();
  });
});
