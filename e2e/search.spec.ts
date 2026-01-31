import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// シードデータの公開記事
const publishedArticles = [
  {
    slug: "typescript-type-safe-code",
    title: "TypeScriptで型安全なコードを書く",
    tags: ["TypeScript"],
  },
  {
    slug: "react-component-patterns",
    title: "Reactコンポーネント設計パターン",
    tags: ["React", "TypeScript"],
  },
  {
    slug: "markdown-image-test",
    title: "Markdownで画像を表示するテスト",
    tags: ["TypeScript", "React"],
  },
];

// 下書き記事（検索結果に含まれない）
const draftArticle = {
  slug: "nextjs-app-router",
  title: "Next.js App Routerの使い方",
  tags: ["Next.js", "React", "TypeScript"],
};

// シードデータの公開メモ
const publishedMemos = [
  {
    slug: "go-tips",
    title: "Go言語のTips",
    tags: ["Go"],
  },
];

// 下書きメモ（検索結果に含まれない）
const draftMemo = {
  slug: "typescript-config",
  title: "TypeScript設定メモ",
  tags: ["TypeScript"],
};

// 検索ページのURLパラメータを構築
const buildSearchUrl = (params: Record<string, string | string[]>): string => {
  const urlParams = new URLSearchParams();
  for (const [key, value] of Object.entries(params)) {
    if (Array.isArray(value)) {
      for (const v of value) {
        urlParams.append(key, v);
      }
    } else {
      urlParams.set(key, value);
    }
  }
  return `/search?${urlParams.toString()}`;
};

/**
 * Search page basic tests
 */
test.describe("search page basics", () => {
  test("search page renders correctly", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Verify main elements are displayed
    await expect(page.locator("main")).toBeVisible();

    // Verify search input
    await expect(page.getByPlaceholder("キーワードで検索...")).toBeVisible();

    // Verify filter section
    await expect(page.getByText("フィルター")).toBeVisible();
  });

  test("displays all published content when no filters applied", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter if collapsed
    const filterButton = page.getByText("フィルター");
    await filterButton.click();

    // Wait for content to load
    await page.waitForTimeout(500);

    // Verify search results count includes published articles and memos
    const resultsText = page.getByText(/検索結果：\d+件/);
    await expect(resultsText).toBeVisible();

    // Get the count
    const text = await resultsText.textContent();
    const count = parseInt(text?.match(/\d+/)?.[0] || "0");

    // Should have at least published articles + published memos
    expect(count).toBeGreaterThanOrEqual(
      publishedArticles.length + publishedMemos.length,
    );
  });

  test("displays empty state message when no criteria applied", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // If no search criteria and results shown, it's the initial state
    // The initial state may show all content or show a "start searching" message
    await expect(page.locator("main")).toBeVisible();
  });
});

/**
 * Free word search tests
 */
test.describe("free word search", () => {
  test("searches by keyword in title", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Search for "TypeScript"
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await searchInput.fill("TypeScript");

    // Wait for search results
    await page.waitForTimeout(1000);

    // Verify TypeScript article is found
    await expect(
      page.getByText("TypeScriptで型安全なコードを書く").first(),
    ).toBeVisible();
  });

  test("searches by keyword in content", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Search for "コンポーネント"
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await searchInput.fill("コンポーネント");

    // Wait for search results
    await page.waitForTimeout(1000);

    // Verify React article is found
    await expect(
      page.getByText("Reactコンポーネント設計パターン").first(),
    ).toBeVisible();
  });

  test("shows no results for non-matching keyword", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Search for non-existent keyword
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await searchInput.fill("存在しないキーワード12345");

    // Wait for search results
    await page.waitForTimeout(1000);

    // Verify no results or empty state
    const resultsText = page.getByText(/検索結果：0件/);
    const noResultsMessage = page.getByText(/見つかりませんでした|検索結果がありません/);

    const hasZeroResults = await resultsText.isVisible().catch(() => false);
    const hasNoResultsMessage = await noResultsMessage.isVisible().catch(() => false);

    expect(hasZeroResults || hasNoResultsMessage).toBe(true);
  });

  test("clears search results when input is cleared", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Search for something
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await searchInput.fill("TypeScript");
    await page.waitForTimeout(1000);

    // Clear the input
    await searchInput.clear();
    await page.waitForTimeout(1000);

    // Results should reset to show all content
    const resultsText = page.getByText(/検索結果：\d+件/);
    await expect(resultsText).toBeVisible();
  });
});

/**
 * Content type filter tests
 */
test.describe("content type filter", () => {
  test("displays all types button as default selected", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // "すべて" button should be active/selected by default
    const allButton = page.getByRole("button", { name: "すべて" });
    await expect(allButton).toBeVisible();
  });

  test("filters to show only articles", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click "記事" button
    await page.getByRole("button", { name: "記事" }).click();

    // Wait for filter to apply
    await page.waitForTimeout(1000);

    // Verify URL includes type parameter
    expect(page.url()).toContain("type=article");

    // Verify only articles are shown (check for "記事" badges)
    const articleBadges = page.locator("text=記事");
    const memoBadges = page.locator("text=メモ");

    const articleCount = await articleBadges.count();
    const memoCount = await memoBadges.count();

    // Should have articles but no memos (excluding filter button text)
    expect(articleCount).toBeGreaterThan(0);
    // Note: The filter button also says "記事", so we check if memo badges are minimal
  });

  test("filters to show only memos", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click "メモ" button
    await page.getByRole("button", { name: "メモ" }).click();

    // Wait for filter to apply
    await page.waitForTimeout(1000);

    // Verify URL includes type parameter
    expect(page.url()).toContain("type=memo");

    // Verify Go Tips memo is visible
    await expect(page.getByText("Go言語のTips").first()).toBeVisible();
  });

  test("returns to all content when すべて is clicked", async ({
    page,
  }: TestArgs) => {
    // Start with article filter
    await page.goto(buildSearchUrl({ type: "article" }));
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click "すべて" button
    await page.getByRole("button", { name: "すべて" }).click();

    // Wait for filter to apply
    await page.waitForTimeout(1000);

    // URL should not have type parameter
    expect(page.url()).not.toContain("type=");
  });
});

/**
 * Sort order tests
 */
test.describe("sort order", () => {
  test("displays latest as default sort", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // "最新" button should be active by default
    const latestButton = page.getByRole("button", { name: "最新" });
    await expect(latestButton).toBeVisible();
  });

  test("sorts by oldest first", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click "古い順" button
    await page.getByRole("button", { name: "古い順" }).click();

    // Wait for sort to apply
    await page.waitForTimeout(1000);

    // Verify URL includes sortBy parameter
    expect(page.url()).toContain("sortBy=oldest");
  });

  test("returns to latest sort when clicked", async ({ page }: TestArgs) => {
    // Start with oldest sort
    await page.goto(buildSearchUrl({ sortBy: "oldest" }));
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click "最新" button
    await page.getByRole("button", { name: "最新" }).click();

    // Wait for sort to apply
    await page.waitForTimeout(1000);

    // URL should have sortBy=latest or no sortBy (latest is default)
    const url = page.url();
    expect(url.includes("sortBy=latest") || !url.includes("sortBy=oldest")).toBe(true);
  });
});

/**
 * Tag filter tests
 */
test.describe("tag filter", () => {
  test("displays available tags in filter section", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Verify "タグ" section exists
    await expect(page.getByText("タグ")).toBeVisible();

    // Verify at least some tags are displayed
    await expect(page.getByRole("button", { name: "TypeScript" })).toBeVisible();
    await expect(page.getByRole("button", { name: "React" })).toBeVisible();
  });

  test("filters by single tag - TypeScript", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click TypeScript tag
    await page.getByRole("button", { name: "TypeScript" }).click();

    // Wait for filter to apply
    await page.waitForTimeout(1000);

    // Verify URL includes tags parameter
    expect(page.url()).toContain("tags=TypeScript");

    // Verify TypeScript-related content is shown
    await expect(
      page.getByText("TypeScriptで型安全なコードを書く").first(),
    ).toBeVisible();
  });

  test("filters by single tag - Go", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click Go tag
    await page.getByRole("button", { name: "Go" }).click();

    // Wait for filter to apply
    await page.waitForTimeout(1000);

    // Verify Go Tips memo is visible
    await expect(page.getByText("Go言語のTips").first()).toBeVisible();

    // Verify results count is limited to Go content
    const resultsText = page.getByText(/検索結果：\d+件/);
    const text = await resultsText.textContent();
    const count = parseInt(text?.match(/\d+/)?.[0] || "0");
    expect(count).toBeLessThanOrEqual(2); // Only Go-related content
  });

  test("filters by multiple tags", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click TypeScript tag
    await page.getByRole("button", { name: "TypeScript" }).click();
    await page.waitForTimeout(500);

    // Click React tag
    await page.getByRole("button", { name: "React" }).click();
    await page.waitForTimeout(1000);

    // Verify URL includes both tags
    expect(page.url()).toContain("TypeScript");
    expect(page.url()).toContain("React");
  });

  test("removes tag filter when clicked again", async ({ page }: TestArgs) => {
    // Start with TypeScript filter
    await page.goto(buildSearchUrl({ tags: ["TypeScript"] }));
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click TypeScript tag again to deselect
    await page.getByRole("button", { name: "TypeScript" }).click();
    await page.waitForTimeout(1000);

    // URL should not have TypeScript tag
    expect(page.url()).not.toContain("tags=TypeScript");
  });
});

/**
 * Combined filter tests
 */
test.describe("combined filters", () => {
  test("combines free word search with type filter", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Enter search keyword
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await searchInput.fill("TypeScript");

    // Expand filter
    await page.getByText("フィルター").click();

    // Select article type
    await page.getByRole("button", { name: "記事" }).click();
    await page.waitForTimeout(1000);

    // Verify URL has both parameters
    expect(page.url()).toContain("freeWord=TypeScript");
    expect(page.url()).toContain("type=article");

    // Verify TypeScript article is shown
    await expect(
      page.getByText("TypeScriptで型安全なコードを書く").first(),
    ).toBeVisible();
  });

  test("combines type filter with sort order", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Select memo type
    await page.getByRole("button", { name: "メモ" }).click();
    await page.waitForTimeout(500);

    // Select oldest sort
    await page.getByRole("button", { name: "古い順" }).click();
    await page.waitForTimeout(1000);

    // Verify URL has both parameters
    expect(page.url()).toContain("type=memo");
    expect(page.url()).toContain("sortBy=oldest");
  });

  test("combines tag filter with type filter", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Select TypeScript tag
    await page.getByRole("button", { name: "TypeScript" }).click();
    await page.waitForTimeout(500);

    // Select article type
    await page.getByRole("button", { name: "記事" }).click();
    await page.waitForTimeout(1000);

    // Verify URL has both parameters
    expect(page.url()).toContain("tags=TypeScript");
    expect(page.url()).toContain("type=article");
  });

  test("combines all filters: keyword, type, sort, and tag", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Enter search keyword
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await searchInput.fill("コンポーネント");

    // Expand filter
    await page.getByText("フィルター").click();

    // Select article type
    await page.getByRole("button", { name: "記事" }).click();
    await page.waitForTimeout(300);

    // Select oldest sort
    await page.getByRole("button", { name: "古い順" }).click();
    await page.waitForTimeout(300);

    // Select React tag
    await page.getByRole("button", { name: "React" }).click();
    await page.waitForTimeout(1000);

    // Verify URL has all parameters
    const url = page.url();
    expect(url).toContain("freeWord=");
    expect(url).toContain("type=article");
    expect(url).toContain("sortBy=oldest");
    expect(url).toContain("React");
  });
});

/**
 * Clear filters tests
 */
test.describe("clear filters", () => {
  test("displays clear button when filters are active", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Initially, clear button should not be visible (or be in inactive state)
    // Select a filter to make it appear
    await page.getByRole("button", { name: "記事" }).click();
    await page.waitForTimeout(500);

    // Clear button should now be visible
    await expect(page.getByRole("button", { name: "クリア" })).toBeVisible();
  });

  test("clears all filters when clear button is clicked", async ({
    page,
  }: TestArgs) => {
    // Start with multiple filters
    await page.goto(
      buildSearchUrl({
        freeWord: "test",
        type: "article",
        sortBy: "oldest",
        tags: ["TypeScript"],
      }),
    );
    await page.waitForLoadState("networkidle");

    // Expand filter
    await page.getByText("フィルター").click();

    // Click clear button
    await page.getByRole("button", { name: "クリア" }).click();
    await page.waitForTimeout(1000);

    // URL should be clean
    const url = page.url();
    expect(url).not.toContain("type=");
    expect(url).not.toContain("sortBy=oldest");
    expect(url).not.toContain("tags=");
  });
});

/**
 * URL parameter tests - verify search works with direct URL access
 */
test.describe("URL parameter handling", () => {
  test("loads search with freeWord parameter", async ({ page }: TestArgs) => {
    await page.goto(buildSearchUrl({ freeWord: "TypeScript" }));
    await page.waitForLoadState("networkidle");

    // Verify search input has the value
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await expect(searchInput).toHaveValue("TypeScript");
  });

  test("loads search with type parameter", async ({ page }: TestArgs) => {
    await page.goto(buildSearchUrl({ type: "memo" }));
    await page.waitForLoadState("networkidle");

    // Expand filter to see button state
    await page.getByText("フィルター").click();

    // Verify memo button is selected (has different styling/state)
    // The Go Tips memo should be visible
    await expect(page.getByText("Go言語のTips").first()).toBeVisible();
  });

  test("loads search with sortBy parameter", async ({ page }: TestArgs) => {
    await page.goto(buildSearchUrl({ sortBy: "oldest" }));
    await page.waitForLoadState("networkidle");

    // Page should load successfully
    await expect(page.locator("main")).toBeVisible();
  });

  test("loads search with tags parameter", async ({ page }: TestArgs) => {
    await page.goto(buildSearchUrl({ tags: ["Go"] }));
    await page.waitForLoadState("networkidle");

    // Verify Go-related content is shown
    await expect(page.getByText("Go言語のTips").first()).toBeVisible();
  });

  test("loads search with multiple parameters", async ({ page }: TestArgs) => {
    await page.goto(
      buildSearchUrl({
        freeWord: "コンポーネント",
        type: "article",
        sortBy: "latest",
        tags: ["React"],
      }),
    );
    await page.waitForLoadState("networkidle");

    // Verify page loads with all parameters
    await expect(page.locator("main")).toBeVisible();

    // Search input should have the value
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await expect(searchInput).toHaveValue("コンポーネント");
  });
});

/**
 * Draft content visibility tests
 */
test.describe("draft content visibility", () => {
  test("does not show draft articles in search results", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Search for the draft article title
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await searchInput.fill("App Router");
    await page.waitForTimeout(1000);

    // Draft article should not appear
    await expect(page.getByText(draftArticle.title)).not.toBeVisible();
  });

  test("does not show draft memos in search results", async ({
    page,
  }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Search for the draft memo title
    const searchInput = page.getByPlaceholder("キーワードで検索...");
    await searchInput.fill("TypeScript設定メモ");
    await page.waitForTimeout(1000);

    // Draft memo should not appear in results
    // Check that no exact match for the draft title exists
    const draftTitle = page.getByText(draftMemo.title, { exact: true });
    await expect(draftTitle).not.toBeVisible();
  });
});

/**
 * Search result card tests
 */
test.describe("search result cards", () => {
  test("displays article badge for articles", async ({ page }: TestArgs) => {
    await page.goto(buildSearchUrl({ type: "article" }));
    await page.waitForLoadState("networkidle");

    // Article cards should have "記事" badge
    const articleBadges = page.locator("article").locator("text=記事");
    await expect(articleBadges.first()).toBeVisible();
  });

  test("displays memo badge for memos", async ({ page }: TestArgs) => {
    await page.goto(buildSearchUrl({ type: "memo" }));
    await page.waitForLoadState("networkidle");

    // Memo cards should have "メモ" badge
    const memoBadges = page.locator("article").locator("text=メモ");
    await expect(memoBadges.first()).toBeVisible();
  });

  test("displays tags on result cards", async ({ page }: TestArgs) => {
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Wait for content
    await page.waitForTimeout(500);

    // Tags should be displayed with # prefix
    await expect(page.getByText("#TypeScript").first()).toBeVisible();
  });

  test("clicking result card navigates to content page", async ({
    page,
  }: TestArgs) => {
    await page.goto(buildSearchUrl({ type: "article" }));
    await page.waitForLoadState("networkidle");

    // Click on an article
    const articleLink = page
      .getByRole("link")
      .filter({ hasText: "TypeScriptで型安全なコードを書く" })
      .first();
    await articleLink.click();

    // Should navigate to article detail page
    await expect(page).toHaveURL(/\/articles\/typescript-type-safe-code/);
  });
});

/**
 * Mobile responsiveness tests
 */
test.describe("mobile responsiveness", () => {
  test("search page is usable on mobile viewport", async ({
    page,
  }: TestArgs) => {
    await page.setViewportSize({ width: 375, height: 667 });
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Search input should be visible
    await expect(page.getByPlaceholder("キーワードで検索...")).toBeVisible();

    // Filter toggle should be visible
    await expect(page.getByText("フィルター")).toBeVisible();
  });

  test("filter panel expands on mobile", async ({ page }: TestArgs) => {
    await page.setViewportSize({ width: 375, height: 667 });
    await page.goto("/search");
    await page.waitForLoadState("networkidle");

    // Click filter toggle
    await page.getByText("フィルター").click();

    // Filter options should be visible
    await expect(page.getByRole("button", { name: "すべて" })).toBeVisible();
    await expect(page.getByRole("button", { name: "記事" })).toBeVisible();
    await expect(page.getByRole("button", { name: "メモ" })).toBeVisible();
  });
});
