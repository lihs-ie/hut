import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// Seed data article slugs
const publishedArticleSlug = "typescript-type-safe-code";
const publishedArticleTitle = "TypeScriptで型安全なコードを書く";
const draftArticleSlug = "nextjs-app-router";
const draftArticleTitle = "Next.js App Routerの使い方";
const multiTagArticleSlug = "react-component-patterns";
const multiTagArticleTitle = "Reactコンポーネント設計パターン";

// Use serial mode to avoid parallel test interference with seed data
test.describe.serial("article edit tests", () => {
  /**
   * Test: Existing article data is displayed correctly.
   */
  test("existing article data is displayed", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`);

    // Verify title is displayed in header input
    const titleInput = page.getByPlaceholder("タイトルを入力");
    await expect(titleInput).toHaveValue(publishedArticleTitle);

    // Verify markdown editor contains content
    const textarea = page.locator("textarea");
    await expect(textarea).toContainText("TypeScriptで型安全なコードを書く");
    await expect(textarea).toContainText("typescript-type-safe-code");

    // Verify TypeScript tag is selected (remove button visible)
    await expect(page.getByRole("button", { name: "TypeScriptを削除" })).toBeVisible();
  });

  /**
   * Test: Draft article shows correct publish switch state.
   */
  test("draft article shows correct state", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${draftArticleSlug}/edit`);

    // Verify title is displayed
    const titleInput = page.getByPlaceholder("タイトルを入力");
    await expect(titleInput).toHaveValue(draftArticleTitle);

    // Verify publish switch is OFF (article is draft)
    const publishSwitch = page.getByRole("checkbox");
    await expect(publishSwitch).not.toBeChecked();

    // Verify button shows "下書き保存"
    await expect(page.getByRole("button", { name: /下書き保存/ })).toBeVisible();
  });

  /**
   * Test: Remove a tag from article (non-destructive - just verifies UI).
   */
  test("remove tag from article", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${multiTagArticleSlug}/edit`);

    // Wait for page to load
    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(multiTagArticleTitle);

    // Verify React tag is selected
    const reactRemoveButton = page.getByRole("button", { name: "Reactを削除" });
    await expect(reactRemoveButton).toBeVisible();

    // Remove the React tag
    await reactRemoveButton.click();

    // Verify React tag is no longer in selected tags
    await expect(reactRemoveButton).not.toBeVisible();

    // Verify React is now in available tags
    await expect(page.getByRole("button", { name: "React" })).toBeVisible();

    // Note: We don't save, so seed data is not affected
  });

  /**
   * Test: Edit article content (non-destructive - just verifies UI).
   */
  test("edit article content", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`);

    // Wait for page to load
    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(publishedArticleTitle);

    // Add content to markdown editor
    const textarea = page.locator("textarea");
    const currentContent = await textarea.inputValue();
    await textarea.fill(currentContent + "\n\n## E2Eテストで追加したセクション\n\nこれはテストです。");

    // Verify content was added
    await expect(textarea).toContainText("E2Eテストで追加したセクション");

    // Add another tag if available
    const goTagButton = page.getByRole("button", { name: "Go" });
    if (await goTagButton.isVisible()) {
      await goTagButton.click();
      await expect(page.getByRole("button", { name: "Goを削除" })).toBeVisible();
    }

    // Verify save button is enabled (we don't actually save to preserve seed data)
    // Button text depends on current publish state
    await expect(page.getByRole("button", { name: /下書き保存|公開する/ })).toBeEnabled();
  });

  /**
   * Test: Toggle publish switch changes button text.
   */
  test("toggle publish switch changes button", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${draftArticleSlug}/edit`);

    // Verify it's a draft
    const publishSwitch = page.getByRole("checkbox");
    await expect(publishSwitch).not.toBeChecked();
    await expect(page.getByRole("button", { name: /下書き保存/ })).toBeVisible();

    // Enable publish switch - use JavaScript to toggle
    await page.getByPlaceholder("タイトルを入力").scrollIntoViewIfNeeded();
    await publishSwitch.evaluate((element: HTMLInputElement) => {
      element.click();
    });

    // Verify switch is now checked
    await expect(publishSwitch).toBeChecked();

    // Verify button changed to "公開する"
    await expect(page.getByRole("button", { name: /公開する/ })).toBeVisible();

    // Toggle back to draft
    await publishSwitch.evaluate((element: HTMLInputElement) => {
      element.click();
    });

    // Verify button changed back to "下書き保存"
    await expect(page.getByRole("button", { name: /下書き保存/ })).toBeVisible();
  });
});
