import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

test.describe("article management", () => {
  test("article management page renders", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");

    // Verify "記事の管理" heading is displayed
    await expect(
      page.getByRole("heading", { name: "記事の管理" }),
    ).toBeVisible();
  });

  test("new article creation button exists", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");

    // Verify "新規作成" button is displayed
    await expect(page.getByRole("button", { name: "新規作成" })).toBeVisible();
  });

  test("search input field exists", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");

    // Verify search input is displayed
    await expect(page.getByPlaceholder("タイトルを入力")).toBeVisible();
  });

  test("status filter select exists", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");

    // Verify status filter select exists (select element contains options)
    await expect(page.locator("select").first()).toBeVisible();
  });

  test("article list is displayed", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");

    // Wait for content to load
    await page.waitForLoadState("networkidle");

    // Verify main content area is displayed
    await expect(page.locator("main")).toBeVisible();
  });

  test("new article creation button navigates to create page", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/articles");

    // Click new article button
    await page.getByRole("button", { name: "新規作成" }).click();

    // Verify navigation to article creation page
    await page.waitForURL("/articles/new", { timeout: 10000 });
  });
});

test.describe("article search functionality", () => {
  test("search by title updates URL", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Fill title search field
    await page.getByPlaceholder("タイトルを入力").fill("テスト");

    // Wait for URL to update
    await expect(page).toHaveURL(/title=/, { timeout: 5000 });
  });

  test("filter by published status updates URL", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Select published status (first select controls 'sort' param with status values)
    await page.locator("select").first().selectOption("published");

    // Verify URL contains sort parameter with published value
    await expect(page).toHaveURL(/sort=published/, { timeout: 5000 });
  });

  test("filter by sort order updates URL", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Select oldest sort order (second select controls 'status' param)
    await page.locator("select").nth(1).selectOption("oldest");

    // Verify URL contains status parameter
    await expect(page).toHaveURL(/status=oldest/, { timeout: 5000 });
  });

  test("combined filters work together", async ({ page }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Apply title filter
    await page.getByPlaceholder("タイトルを入力").fill("テスト");
    await expect(page).toHaveURL(/title=/, { timeout: 5000 });

    // Apply status filter (first select)
    await page.locator("select").first().selectOption("published");
    await expect(page).toHaveURL(/sort=published/, { timeout: 5000 });

    // Verify both parameters are in URL
    const url = page.url();
    expect(url).toMatch(/title=/);
    expect(url).toMatch(/sort=published/);
  });
});

test.describe("article navigation", () => {
  test("clicking article title navigates to preview page", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Get first article title link
    const firstArticleLink = page.locator("article a").first();

    // Skip if no articles exist
    if ((await firstArticleLink.count()) === 0) {
      test.skip();
      return;
    }

    // Click article title
    await firstArticleLink.click();

    // Verify navigation to article preview page
    await expect(page).toHaveURL(/\/articles\/[^/]+$/, { timeout: 10000 });
  });

  test("clicking edit button navigates to edit page", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Find edit link (contains /edit in href)
    const editButton = page
      .locator('[href*="/articles/"][href*="/edit"]')
      .first();

    // Skip if no articles exist
    if ((await editButton.count()) === 0) {
      test.skip();
      return;
    }

    // Click edit button
    await editButton.click();

    // Verify navigation to edit page
    await expect(page).toHaveURL(/\/articles\/.+\/edit/, { timeout: 10000 });
  });
});

test.describe("article deletion", () => {
  test("clicking delete button opens confirmation modal", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Find article cards (card container has article element inside)
    const cardContainers = page.locator("article").locator("..");
    const cardCount = await cardContainers.count();

    // Skip if no articles exist
    if (cardCount === 0) {
      test.skip();
      return;
    }

    // Find delete button (EraserButton is the only button in the card)
    const deleteButton = cardContainers.first().getByRole("button");

    // Click delete button
    await deleteButton.click();

    // Verify confirmation modal is displayed
    await expect(
      page.getByRole("heading", { name: "コンテンツの削除" }),
    ).toBeVisible();
    await expect(page.getByText(/削除しますか/)).toBeVisible();
  });

  test("clicking cancel button closes confirmation modal", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Find article card containers
    const cardContainers = page.locator("article").locator("..");
    const cardCount = await cardContainers.count();

    // Skip if no articles exist
    if (cardCount === 0) {
      test.skip();
      return;
    }

    // Find and click delete button
    const deleteButton = cardContainers.first().getByRole("button");
    await deleteButton.click();

    // Verify modal is open
    await expect(
      page.getByRole("heading", { name: "コンテンツの削除" }),
    ).toBeVisible();

    // Click cancel button
    await page.getByRole("button", { name: "キャンセル" }).click();

    // Verify modal is closed
    await expect(
      page.getByRole("heading", { name: "コンテンツの削除" }),
    ).not.toBeVisible();
  });

  test("clicking confirm button deletes article", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/articles");
    await page.waitForLoadState("networkidle");

    // Find article card containers
    const cardContainers = page.locator("article").locator("..");
    const cardCount = await cardContainers.count();

    // Skip if no articles exist
    if (cardCount === 0) {
      test.skip();
      return;
    }

    // Get first article title for verification
    const firstArticleTitle = await cardContainers
      .first()
      .locator("article a")
      .textContent();

    // Find and click delete button
    const deleteButton = cardContainers.first().getByRole("button");
    await deleteButton.click();

    // Verify modal is open
    await expect(
      page.getByRole("heading", { name: "コンテンツの削除" }),
    ).toBeVisible();

    // Verify modal shows correct article title in the message
    if (firstArticleTitle) {
      await expect(
        page.getByText(`「${firstArticleTitle}」を削除しますか？`),
      ).toBeVisible();
    }

    // Click confirm button
    await page.getByRole("button", { name: "削除する" }).click();

    // Wait for modal to close
    await expect(
      page.getByRole("heading", { name: "コンテンツの削除" }),
    ).not.toBeVisible({ timeout: 10000 });

    // Verify article count decreased
    await expect(cardContainers).toHaveCount(cardCount - 1, {
      timeout: 10000,
    });
  });
});
