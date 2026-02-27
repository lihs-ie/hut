import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const newArticlePath = "/articles/new";

/**
 * Generate a unique slug for test articles.
 */
const generateTestSlug = (): string => {
  const timestamp = Date.now();
  return `e2e-test-article-${timestamp}`;
};

/**
 *
 * Create frontmatter content for a new article.
 */
const createFrontmatter = (options: {
  title: string;
  excerpt: string;
  slug: string;
  tags?: string[];
}): string => {
  const tagsArray = options.tags ?? [];
  return `---
title: ${options.title}
excerpt: ${options.excerpt}
slug: ${options.slug}
tags: [${tagsArray.join(", ")}]
---

`;
};

/**
 * Test: Save a new article as draft.
 */
const saveNewArticleAsDraft = async ({ page }: TestArgs): Promise<void> => {
  const testSlug = generateTestSlug();
  const testTitle = "E2Eテスト記事（下書き）";

  await page.goto(newArticlePath);

  // Enter title in header
  await page.getByPlaceholder("タイトルを入力").fill(testTitle);

  // Edit markdown content with frontmatter
  const textarea = page.locator("textarea");
  const frontmatter = createFrontmatter({
    title: testTitle,
    excerpt: "E2Eテストで作成した下書き記事です。",
    slug: testSlug,
  });
  await textarea.fill(
    frontmatter + "# テスト記事\n\nこれはテスト用の本文です。",
  );

  // Select a tag (TypeScript)
  await page.getByRole("button", { name: "TypeScript" }).click();

  // Verify tag is selected (should show in selected tags section)
  await expect(
    page.getByRole("button", { name: "TypeScriptを削除" }),
  ).toBeVisible();

  // Save as draft
  await page.getByRole("button", { name: /下書き保存/ }).click();

  // Wait for save to complete (draft does not redirect, just wait for button to be enabled again)
  await expect(page.getByRole("button", { name: /下書き保存/ })).toBeEnabled({
    timeout: 10000,
  });
};

/**
 * Test: Publish a new article.
 */
const publishNewArticle = async ({ page }: TestArgs): Promise<void> => {
  const testSlug = generateTestSlug();
  const testTitle = "E2Eテスト記事（公開）";

  await page.goto(newArticlePath);

  // Enter title in header
  await page.getByPlaceholder("タイトルを入力").fill(testTitle);

  // Edit markdown content with frontmatter
  const textarea = page.locator("textarea");
  const frontmatter = createFrontmatter({
    title: testTitle,
    excerpt: "E2Eテストで作成した公開記事です。",
    slug: testSlug,
  });
  await textarea.fill(
    frontmatter + "# 公開テスト記事\n\nこれは公開テスト用の本文です。",
  );

  // Select a tag
  await page.getByRole("button", { name: "React" }).click();

  // Enable publish switch - scroll header into view first, then use JavaScript to check
  const publishCheckbox = page.getByRole("checkbox", { name: "公開" });
  await page.getByPlaceholder("タイトルを入力").scrollIntoViewIfNeeded();
  await publishCheckbox.evaluate((element: HTMLInputElement) => {
    element.click();
  });

  // Verify checkbox is now checked
  await expect(publishCheckbox).toBeChecked();

  // Verify button text changed to "公開する"
  const publishButton = page.getByRole("button", { name: /公開する/ });
  await expect(publishButton).toBeVisible();

  // Publish the article
  await publishButton.click();

  // Wait for navigation (either to article detail or back to articles list)
  await page.waitForURL(/\/articles\//, { timeout: 15000 });
};

/**
 * Test: Save button is disabled when title is empty.
 */
const saveButtonDisabledWithoutTitle = async ({
  page,
}: TestArgs): Promise<void> => {
  await page.goto(newArticlePath);

  // Verify title input is empty
  const titleInput = page.getByPlaceholder("タイトルを入力");
  await expect(titleInput).toHaveValue("");

  // Verify save button is disabled
  const saveButton = page.getByRole("button", { name: /下書き保存/ });
  await expect(saveButton).toBeDisabled();

  // Enter a title
  await titleInput.fill("テストタイトル");

  // Verify save button is now enabled
  await expect(saveButton).toBeEnabled();

  // Clear the title
  await titleInput.fill("");

  // Verify save button is disabled again
  await expect(saveButton).toBeDisabled();
};

/**
 * Test: Tags can be selected and cleared.
 */
const tagSelectionWorks = async ({ page }: TestArgs): Promise<void> => {
  await page.goto(newArticlePath);

  // Verify available tags section exists
  await expect(page.getByText("クリックして追加")).toBeVisible();

  // Select multiple tags
  await page.getByRole("button", { name: "TypeScript" }).click();
  await page.getByRole("button", { name: "React" }).click();

  // Verify tags are selected (remove buttons visible)
  await expect(
    page.getByRole("button", { name: "TypeScriptを削除" }),
  ).toBeVisible();
  await expect(page.getByRole("button", { name: "Reactを削除" })).toBeVisible();

  // Verify "全て解除" button is visible
  const clearAllButton = page.getByRole("button", { name: "全て解除" });
  await expect(clearAllButton).toBeVisible();

  // Clear all tags
  await clearAllButton.click();

  // Verify tags are cleared (remove buttons should not be visible)
  await expect(
    page.getByRole("button", { name: "TypeScriptを削除" }),
  ).not.toBeVisible();
  await expect(
    page.getByRole("button", { name: "Reactを削除" }),
  ).not.toBeVisible();

  // Verify available tags are shown again
  await expect(page.getByRole("button", { name: "TypeScript" })).toBeVisible();
};

test("save new article as draft", saveNewArticleAsDraft);
test("publish new article", publishNewArticle);
test("save button disabled without title", saveButtonDisabledWithoutTitle);
test("tag selection works", tagSelectionWorks);
