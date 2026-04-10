import { expect, type Page, test } from "@playwright/test";
import {
  getChapterIdentifierBySlug,
  getContentTokenIndex,
} from "./helpers/search-token";

type TestArgs = {
  page: Page;
};

const seriesSlug = "rust-system-programming";
const chapterSlug = "chapter-1-rust-basics";

test.describe("chapter edit page", () => {
  test("page is accessible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`, { waitUntil: "load" });

    await expect(page.locator("main")).toBeVisible({ timeout: 15000 });
  });

  test("CodeMirror editor is visible", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`, { waitUntil: "load" });

    await expect(
      page.locator(".cm-content"),
    ).toBeVisible({ timeout: 15000 });
  });

  test("save button is present", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`, { waitUntil: "load" });

    await expect(
      page.getByRole("button", { name: /保存|下書き保存|公開する/ }),
    ).toBeVisible({ timeout: 15000 });
  });

  test("image paste inserts placeholder into editor", async ({ page }: TestArgs) => {
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`, { waitUntil: "load" });

    const editorContent = page.locator(".cm-content");
    await editorContent.click();

    const pngBytes = Buffer.from(
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
      "base64",
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
    await page.goto(`/series/${seriesSlug}/chapters/${chapterSlug}/edit`, { waitUntil: "load" });

    const editorContent = page.locator(".cm-content");
    await editorContent.click();

    const pngBytes = Buffer.from(
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
      "base64",
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

    await expect(editorContent).toContainText("uploading...", { timeout: 5000 });
  });
});

test.describe("chapter search token verification", () => {
  test("search tokens exist for published chapter", async () => {
    const chapterIdentifier = await getChapterIdentifierBySlug(chapterSlug);

    if (chapterIdentifier === undefined) {
      throw new Error(
        `Chapter identifier not found for slug: ${chapterSlug}`,
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
