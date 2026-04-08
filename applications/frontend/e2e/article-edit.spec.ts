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
  test("existing article data is displayed", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`, { waitUntil: "load" });

    const titleInput = page.getByPlaceholder("タイトルを入力");
    await expect(titleInput).toHaveValue(publishedArticleTitle);

    const editorContent = page.locator(".cm-content");
    await expect(editorContent).toContainText("TypeScriptで型安全なコードを書く");
    await expect(editorContent).toContainText("typescript-type-safe-code");

    await expect(page.getByRole("button", { name: "TypeScriptを削除" })).toBeVisible();
  });

  test("draft article shows correct state", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${draftArticleSlug}/edit`, { waitUntil: "load" });

    const titleInput = page.getByPlaceholder("タイトルを入力");
    await expect(titleInput).toHaveValue(draftArticleTitle);

    const publishSwitch = page.getByRole("checkbox");
    await expect(publishSwitch).not.toBeChecked();

    await expect(page.getByRole("button", { name: /下書き保存/ })).toBeVisible();
  });

  test("remove tag from article", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${multiTagArticleSlug}/edit`, { waitUntil: "load" });

    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(multiTagArticleTitle);

    const reactRemoveButton = page.getByRole("button", { name: "Reactを削除" });
    await expect(reactRemoveButton).toBeVisible();

    await reactRemoveButton.click();

    await expect(reactRemoveButton).not.toBeVisible();

    await expect(page.getByRole("button", { name: "React" })).toBeVisible();
  });

  test("edit article content", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`, { waitUntil: "load" });

    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(publishedArticleTitle);

    const editorContent = page.locator(".cm-content");
    await editorContent.click();
    await page.keyboard.press("Control+End");
    await page.keyboard.type("\n\n## E2Eテストで追加したセクション\n\nこれはテストです。");

    await expect(editorContent).toContainText("E2Eテストで追加したセクション");

    await expect(page.getByRole("button", { name: /下書き保存|公開する/ })).toBeEnabled();
  });

  test("toggle publish switch changes button", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${draftArticleSlug}/edit`, { waitUntil: "load" });

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

  test("editor toolbar is visible", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`, { waitUntil: "load" });

    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(publishedArticleTitle);

    await expect(page.getByRole("button", { name: /太字/ })).toBeVisible();
    await expect(page.getByRole("button", { name: /斜体/ })).toBeVisible();
    await expect(page.getByRole("button", { name: /リンク/ })).toBeVisible();
  });

  test("image paste inserts placeholder into editor", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`, { waitUntil: "load" });

    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(publishedArticleTitle);

    const editorContent = page.locator(".cm-content");
    await editorContent.click();

    const pngBytes = Buffer.from(
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
      "base64"
    );

    await page.evaluate((imageData) => {
      const uint8Array = new Uint8Array(imageData);
      const blob = new Blob([uint8Array], { type: "image/png" });
      const file = new File([blob], "test.png", { type: "image/png" });
      const dataTransfer = new DataTransfer();
      dataTransfer.items.add(file);

      const clipboardEvent = new ClipboardEvent("paste", {
        clipboardData: dataTransfer,
        bubbles: true,
        cancelable: true,
      });

      const editorElement = document.querySelector(".cm-content");
      if (editorElement) {
        editorElement.dispatchEvent(clipboardEvent);
      }
    }, Array.from(pngBytes));

    await expect(editorContent).toContainText("uploading...", { timeout: 5000 });
  });

  test("image drag-and-drop inserts placeholder into editor", async ({ page }: TestArgs) => {
    await page.goto(`/articles/${publishedArticleSlug}/edit`, { waitUntil: "load" });

    await expect(page.getByPlaceholder("タイトルを入力")).toHaveValue(publishedArticleTitle);

    const editorContainer = page.locator(".cm-content");
    await editorContainer.click();

    const pngBytes = Buffer.from(
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
      "base64"
    );

    await page.evaluate((imageData) => {
      const uint8Array = new Uint8Array(imageData);
      const blob = new Blob([uint8Array], { type: "image/png" });
      const file = new File([blob], "test.png", { type: "image/png" });
      const dataTransfer = new DataTransfer();
      dataTransfer.items.add(file);

      const editorWrapper = document.querySelector(".cm-editor")?.parentElement;
      if (editorWrapper) {
        const dropEvent = new DragEvent("drop", {
          dataTransfer,
          bubbles: true,
          cancelable: true,
        });
        editorWrapper.dispatchEvent(dropEvent);
      }
    }, Array.from(pngBytes));

    await expect(editorContainer).toContainText("uploading...", { timeout: 5000 });
  });
});
