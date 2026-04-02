import { expect, type Page, test } from "@playwright/test";
import {
  getArticleIdentifierBySlug,
  getContentTokenIndex,
} from "./helpers/search-token";

type TestArgs = {
  page: Page;
};

const newArticlePath = "/articles/new";

const generateTestSlug = (): string => {
  const timestamp = Date.now();
  return `e2e-test-article-${timestamp}`;
};

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

const fillCodeMirrorEditor = async (page: Page, content: string): Promise<void> => {
  const editorContent = page.locator(".cm-content");
  await editorContent.click();
  await page.keyboard.press("Control+a");
  await page.keyboard.type(content);
};

const saveNewArticleAsDraft = async ({ page }: TestArgs): Promise<void> => {
  const testSlug = generateTestSlug();
  const testTitle = "E2Eテスト記事（下書き）";

  await page.goto(newArticlePath);

  await page.getByPlaceholder("タイトルを入力").fill(testTitle);

  const frontmatter = createFrontmatter({
    title: testTitle,
    excerpt: "E2Eテストで作成した下書き記事です。",
    slug: testSlug,
  });
  await fillCodeMirrorEditor(
    page,
    frontmatter + "# テスト記事\n\nこれはテスト用の本文です。",
  );

  await page.getByRole("button", { name: "TypeScript" }).click();

  await expect(
    page.getByRole("button", { name: "TypeScriptを削除" }),
  ).toBeVisible();

  await page.getByRole("button", { name: /下書き保存/ }).click();

  await expect(page.getByRole("button", { name: /下書き保存/ })).toBeEnabled({
    timeout: 10000,
  });
};

const publishNewArticle = async ({ page }: TestArgs): Promise<void> => {
  const testSlug = generateTestSlug();
  const testTitle = "E2Eテスト記事（公開）";

  await page.goto(newArticlePath);

  await page.getByPlaceholder("タイトルを入力").fill(testTitle);

  const frontmatter = createFrontmatter({
    title: testTitle,
    excerpt: "E2Eテストで作成した公開記事です。",
    slug: testSlug,
  });
  await fillCodeMirrorEditor(
    page,
    frontmatter + "# 公開テスト記事\n\nこれは公開テスト用の本文です。",
  );

  await page.getByRole("button", { name: "React" }).click();

  const publishCheckbox = page.getByRole("checkbox");
  await page.getByPlaceholder("タイトルを入力").scrollIntoViewIfNeeded();
  await publishCheckbox.evaluate((element: HTMLInputElement) => {
    element.click();
  });

  await expect(publishCheckbox).toBeChecked();

  const publishButton = page.getByRole("button", { name: /公開する/ });
  await expect(publishButton).toBeVisible();

  await publishButton.click();

  await page.waitForURL(/\/articles\//, { timeout: 15000 });
};

const saveButtonDisabledWithoutTitle = async ({
  page,
}: TestArgs): Promise<void> => {
  await page.goto(newArticlePath);

  const titleInput = page.getByPlaceholder("タイトルを入力");
  await expect(titleInput).toHaveValue("");

  const saveButton = page.getByRole("button", { name: /下書き保存/ });
  await expect(saveButton).toBeDisabled();

  await titleInput.fill("テストタイトル");

  await expect(saveButton).toBeEnabled();

  await titleInput.fill("");

  await expect(saveButton).toBeDisabled();
};

const tagSelectionWorks = async ({ page }: TestArgs): Promise<void> => {
  await page.goto(newArticlePath);

  await expect(page.getByText("クリックして追加")).toBeVisible();

  await page.getByRole("button", { name: "TypeScript" }).click();
  await page.getByRole("button", { name: "React" }).click();

  await expect(
    page.getByRole("button", { name: "TypeScriptを削除" }),
  ).toBeVisible();
  await expect(page.getByRole("button", { name: "Reactを削除" })).toBeVisible();

  const clearAllButton = page.getByRole("button", { name: "全て解除" });
  await expect(clearAllButton).toBeVisible();

  await clearAllButton.click();

  await expect(
    page.getByRole("button", { name: "TypeScriptを削除" }),
  ).not.toBeVisible();
  await expect(
    page.getByRole("button", { name: "Reactを削除" }),
  ).not.toBeVisible();

  await expect(page.getByRole("button", { name: "TypeScript" })).toBeVisible();
};

test("save new article as draft", saveNewArticleAsDraft);
test("publish new article", publishNewArticle);
test("save button disabled without title", saveButtonDisabledWithoutTitle);
test("tag selection works", tagSelectionWorks);

test.describe("search token verification", () => {
  const existingArticleSlug = "typescript-type-safe-code";

  test("search tokens exist for seed article", async () => {
    const articleIdentifier =
      await getArticleIdentifierBySlug(existingArticleSlug);

    if (articleIdentifier === undefined) {
      test.skip(true, "seed article not found in Firestore");
      return;
    }

    const tokenIndex = await getContentTokenIndex(
      "article",
      articleIdentifier,
    );

    expect(tokenIndex).toBeDefined();

    if (tokenIndex === undefined) {
      return;
    }

    expect(tokenIndex.tokens.length).toBeGreaterThan(0);
  });
});
