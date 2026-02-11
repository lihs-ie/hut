import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// Firestore Emulator REST API configuration
const FIRESTORE_EMULATOR_HOST = "http://localhost:8085";
const PROJECT_ID = "demo-hut";
const DATABASE_ID = "(default)";
const BASE_URL = `${FIRESTORE_EMULATOR_HOST}/v1/projects/${PROJECT_ID}/databases/${DATABASE_ID}/documents`;

// シードデータのタグ
const seedTags = [
  { name: "TypeScript" },
  { name: "React" },
  { name: "Next.js" },
  { name: "Go" },
  { name: "Rust" },
];

/**
 * Get all tags from Firestore Emulator
 */
async function getTagsFromFirestore(): Promise<
  Array<{
    name: string;
    identifier: string;
    logo: string;
  }>
> {
  const url = `${BASE_URL}/tags`;
  const response = await fetch(url);

  if (!response.ok) {
    return [];
  }

  const data = (await response.json()) as {
    documents?: Array<{
      name: string;
      fields: {
        identifier: { stringValue: string };
        name: { stringValue: string };
        logo: { stringValue: string };
      };
    }>;
  };

  return (data.documents || []).map((doc) => ({
    name: doc.fields.name.stringValue,
    identifier: doc.fields.identifier.stringValue,
    logo: doc.fields.logo.stringValue,
  }));
}

/**
 * Get a specific tag by name from Firestore
 */
async function getTagByName(
  tagName: string,
): Promise<{ identifier: string; name: string; logo: string } | null> {
  const tags = await getTagsFromFirestore();
  return tags.find((tag) => tag.name === tagName) || null;
}

/**
 * Delete a tag from Firestore by identifier
 */
async function _deleteTagFromFirestore(identifier: string): Promise<void> {
  const url = `${FIRESTORE_EMULATOR_HOST}/v1/projects/${PROJECT_ID}/databases/${DATABASE_ID}/documents/tags/${identifier}`;
  await fetch(url, { method: "DELETE" });
}

/**
 * Delete tag name index from Firestore
 */
async function _deleteTagNameIndex(tagName: string): Promise<void> {
  const url = `${FIRESTORE_EMULATOR_HOST}/v1/projects/${PROJECT_ID}/databases/${DATABASE_ID}/documents/index/tags/name/${tagName}`;
  await fetch(url, { method: "DELETE" });
}

/**
 * Create a tag in Firestore (for cleanup/restoration)
 * NOTE: identifier must be a valid ULID format
 * NOTE: timeline dates must be stored as stringValue (not timestampValue) to match the infrastructure layer
 */
async function _createTagInFirestore(
  identifier: string,
  name: string,
  logo: string,
): Promise<void> {
  const url = `${BASE_URL}/tags?documentId=${identifier}`;
  // Use ISO string format for dates - must match infrastructure layer format
  const now = new Date().toISOString();

  const body = {
    fields: {
      identifier: { stringValue: identifier },
      name: { stringValue: name },
      logo: { stringValue: logo },
      timeline: {
        mapValue: {
          fields: {
            // IMPORTANT: Use stringValue, not timestampValue, to match the infrastructure layer
            createdAt: { stringValue: now },
            updatedAt: { stringValue: now },
          },
        },
      },
      version: { integerValue: "1" },
    },
  };

  await fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
}

/**
 * Create tag name index in Firestore
 * NOTE: Index uses timestampValue for createdAt (matches seed script)
 */
async function _createTagNameIndex(
  tagName: string,
  identifier: string,
): Promise<void> {
  const url = `${BASE_URL}/index/tags/name?documentId=${tagName}`;
  const now = new Date().toISOString();

  const body = {
    fields: {
      referenceIdentifier: { stringValue: identifier },
      // Index uses timestampValue (matches seed script with useTimestamp: true)
      createdAt: { timestampValue: now },
    },
  };

  await fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
}

/**
 * Tag CRUD tests with Firestore verification
 * These tests verify data persistence and perform cleanup to maintain seed data
 */
test.describe.serial("tag CRUD operations with Firestore", () => {
  // Test data for new tag (max 20 chars)
  const testTagName = `E2E_${Date.now().toString().slice(-8)}`;
  const testTagLogo =
    "https://raw.githubusercontent.com/devicons/devicon/master/icons/python/python-original.svg";
  let createdTagIdentifier: string | null = null;

  test.describe("tag creation", () => {
    test("displays tag creation form correctly", async ({ page }: TestArgs) => {
      await page.goto("/admin/tags/new");

      // Verify form elements
      await expect(
        page.getByRole("heading", { name: "新規タグ作成" }),
      ).toBeVisible();
      await expect(page.getByPlaceholder("例: Next.js")).toBeVisible();
      await expect(
        page.getByPlaceholder("https://example.com/logo.png"),
      ).toBeVisible();
      await expect(
        page.getByRole("button", { name: "作成する" }),
      ).toBeVisible();
      await expect(
        page.getByRole("button", { name: "キャンセル" }),
      ).toBeVisible();
    });

    test("create button is disabled when name is empty", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/tags/new");

      const createButton = page.getByRole("button", { name: "作成する" });
      await expect(createButton).toBeDisabled();
    });

    test("create button is enabled when name and logo are filled", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/tags/new");

      await page.getByPlaceholder("例: Next.js").fill("テストタグ");
      await page
        .getByPlaceholder("https://example.com/logo.png")
        .fill("https://example.com/test.svg");

      const createButton = page.getByRole("button", { name: "作成する" });
      await expect(createButton).toBeEnabled();
    });

    test("creates new tag and verifies in Firestore", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/tags/new");
      await page.waitForLoadState("networkidle");

      const nameInput = page.getByPlaceholder("例: Next.js");
      await expect(nameInput).toBeVisible();
      await expect(nameInput).toBeEditable();

      await nameInput.fill(testTagName);

      const logoInput = page.getByPlaceholder("https://example.com/logo.png");
      await logoInput.fill(testTagLogo);

      await expect(nameInput).toHaveValue(testTagName);
      await expect(logoInput).toHaveValue(testTagLogo);

      const createButton = page.getByRole("button", { name: "作成する" });
      await expect(createButton).toBeEnabled();

      await createButton.click();

      await page.waitForURL(/\/admin\/tags$/, { timeout: 15000 });

      // Verify tag appears in the list
      await expect(page.getByText(testTagName)).toBeVisible();

      // Verify in Firestore
      const createdTag = await getTagByName(testTagName);
      expect(createdTag).not.toBeNull();
      expect(createdTag?.name).toBe(testTagName);

      // Store identifier for later tests
      createdTagIdentifier = createdTag?.identifier ?? null;
    });

    test("new tag appears in tag list and can be searched", async ({
      page,
    }: TestArgs) => {
      await page.goto("/admin/tags");
      await page.waitForLoadState("networkidle");

      // Search for the created tag
      await page.getByPlaceholder("タグ名で検索").fill(testTagName);

      // Wait for search results
      await page.waitForTimeout(500);

      // Verify tag is found
      await expect(page.getByText(testTagName)).toBeVisible();
    });
  });

  test.describe("tag editing", () => {
    const updatedTagName = `更新済みタグ_${Date.now()}`;
    const updatedTagLogo =
      "https://raw.githubusercontent.com/devicons/devicon/master/icons/java/java-original.svg";

    test("navigates to edit page from tag list", async ({ page }: TestArgs) => {
      test.skip(!createdTagIdentifier, "No tag was created in previous test");

      await page.goto("/admin/tags");
      await page.waitForLoadState("networkidle");

      // Search for the tag
      await page.getByPlaceholder("タグ名で検索").fill(testTagName);
      await page.waitForTimeout(500);

      // Click edit link (the row should be clickable or have an edit button)
      const tagRow = page.getByText(testTagName).first();
      await tagRow.click();

      // Should navigate to edit page
      await page.waitForURL(/\/admin\/tags\/.*\/edit/, { timeout: 10000 });
    });

    test("displays tag edit form with existing data", async ({
      page,
    }: TestArgs) => {
      test.skip(!createdTagIdentifier, "No tag was created in previous test");

      await page.goto(`/admin/tags/${createdTagIdentifier}/edit`);
      await page.waitForLoadState("networkidle");

      // Verify form heading
      await expect(
        page.getByRole("heading", { name: "タグ編集" }),
      ).toBeVisible();

      // Verify existing data is populated
      const nameInput = page.getByPlaceholder("例: Next.js");
      await expect(nameInput).toHaveValue(testTagName);

      const logoInput = page.getByPlaceholder("https://example.com/logo.png");
      await expect(logoInput).toHaveValue(testTagLogo);

      // Verify update and delete buttons
      await expect(
        page.getByRole("button", { name: "更新する" }),
      ).toBeVisible();
      await expect(
        page.getByRole("button", { name: "タグを削除" }),
      ).toBeVisible();
    });

    test("updates tag and verifies in Firestore", async ({
      page,
    }: TestArgs) => {
      test.skip(!createdTagIdentifier, "No tag was created in previous test");

      await page.goto(`/admin/tags/${createdTagIdentifier}/edit`);
      await page.waitForLoadState("networkidle");

      // Clear and update name
      const nameInput = page.getByPlaceholder("例: Next.js");
      await nameInput.clear();
      await nameInput.fill(updatedTagName);

      // Clear and update logo
      const logoInput = page.getByPlaceholder("https://example.com/logo.png");
      await logoInput.clear();
      await logoInput.fill(updatedTagLogo);

      // Click update button
      await page.getByRole("button", { name: "更新する" }).click();

      // Wait for update to complete
      await page.waitForURL("/admin/tags", { timeout: 10000 });

      // Verify updated tag appears in the list
      await expect(page.getByText(updatedTagName)).toBeVisible();

      // Verify in Firestore
      const updatedTag = await getTagByName(updatedTagName);
      expect(updatedTag).not.toBeNull();
      expect(updatedTag?.name).toBe(updatedTagName);
      expect(updatedTag?.logo).toBe(updatedTagLogo);

      // Old name should no longer exist
      const oldTag = await getTagByName(testTagName);
      expect(oldTag).toBeNull();
    });

    test("cancel button returns to tag list without saving", async ({
      page,
    }: TestArgs) => {
      test.skip(!createdTagIdentifier, "No tag was created in previous test");

      await page.goto(`/admin/tags/${createdTagIdentifier}/edit`);
      await page.waitForLoadState("networkidle");

      // Modify name but don't save
      const nameInput = page.getByPlaceholder("例: Next.js");
      await nameInput.clear();
      await nameInput.fill("キャンセルテスト");

      // Click cancel
      await page.getByRole("button", { name: "キャンセル" }).click();

      // Should return to tag list
      await page.waitForURL("/admin/tags", { timeout: 10000 });

      // Original updated name should still be visible (not "キャンセルテスト")
      await expect(
        page.getByText(updatedTagName).or(page.getByText(testTagName)),
      ).toBeVisible();
    });
  });

  test.describe("tag deletion", () => {
    test("displays delete confirmation modal", async ({ page }: TestArgs) => {
      test.skip(!createdTagIdentifier, "No tag was created in previous test");

      await page.goto(`/admin/tags/${createdTagIdentifier}/edit`);
      await page.waitForLoadState("networkidle");

      // Click delete button
      await page.getByRole("button", { name: "タグを削除" }).click();

      // Verify confirmation modal appears (modal uses heading, not dialog role)
      await expect(
        page.getByRole("heading", { name: "タグを削除", level: 2 }),
      ).toBeVisible();
      await expect(page.getByText("この操作は取り消せません")).toBeVisible();
      await expect(page.getByRole("button", { name: "削除する" })).toBeVisible();
    });

    test("cancel button closes modal without deleting", async ({
      page,
    }: TestArgs) => {
      test.skip(!createdTagIdentifier, "No tag was created in previous test");

      await page.goto(`/admin/tags/${createdTagIdentifier}/edit`);
      await page.waitForLoadState("networkidle");

      // Click delete button
      await page.getByRole("button", { name: "タグを削除" }).click();

      // Wait for modal to appear
      const modalHeading = page.getByRole("heading", {
        name: "タグを削除",
        level: 2,
      });
      await expect(modalHeading).toBeVisible();

      // Click cancel in modal (there are multiple cancel buttons, get the one in the modal)
      const cancelButtons = page.getByRole("button", { name: "キャンセル" });
      await cancelButtons.last().click();

      // Modal should close
      await expect(modalHeading).not.toBeVisible();

      // Verify tag still exists in Firestore
      const tags = await getTagsFromFirestore();
      const tagExists = tags.some((t) => t.identifier === createdTagIdentifier);
      expect(tagExists).toBe(true);
    });

    test("deletes tag and verifies removal from Firestore", async ({
      page,
    }: TestArgs) => {
      test.skip(!createdTagIdentifier, "No tag was created in previous test");

      await page.goto(`/admin/tags/${createdTagIdentifier}/edit`);
      await page.waitForLoadState("networkidle");

      // Get current tag name for verification
      const nameInput = page.getByPlaceholder("例: Next.js");
      const currentTagName = await nameInput.inputValue();

      // Click delete button
      await page.getByRole("button", { name: "タグを削除" }).click();

      // Wait for modal to appear
      const modalHeading = page.getByRole("heading", { name: "タグを削除", level: 2 });
      await expect(modalHeading).toBeVisible();

      // Confirm deletion in modal
      const confirmButton = page.getByRole("button", { name: "削除する" });
      await confirmButton.click();

      // Wait for the action to complete
      await page.waitForURL("/admin/tags", { timeout: 10000 });

      // Verify tag no longer appears in the list
      await expect(page.getByText(currentTagName)).not.toBeVisible();

      // Wait a bit for Firestore to sync
      await page.waitForTimeout(2000);

      // Verify removal from Firestore by identifier
      const tags = await getTagsFromFirestore();
      const tagExists = tags.some((t) => t.identifier === createdTagIdentifier);
      expect(tagExists).toBe(false);

      // Clear the identifier since tag is deleted
      createdTagIdentifier = null;
    });
  });
});

/**
 * Tag list page tests
 */
test.describe("tag list page", () => {
  test("displays all seed tags", async ({ page }: TestArgs) => {
    await page.goto("/admin/tags");
    await page.waitForLoadState("networkidle");

    // Verify all seed tags are displayed
    for (const tag of seedTags) {
      await expect(page.getByText(tag.name).first()).toBeVisible();
    }
  });

  test("search filters tags by name", async ({ page }: TestArgs) => {
    await page.goto("/admin/tags");
    await page.waitForLoadState("networkidle");

    // Search for TypeScript
    await page.getByPlaceholder("タグ名で検索").fill("Type");
    await page.waitForTimeout(500);

    // TypeScript should be visible
    await expect(page.getByText("TypeScript").first()).toBeVisible();

    // Other tags should not be visible
    await expect(page.getByText("React")).not.toBeVisible();
    await expect(page.getByText("Go")).not.toBeVisible();
  });

  test("search shows no results message for non-existent tag", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/tags");
    await page.waitForLoadState("networkidle");

    // Search for non-existent tag
    await page.getByPlaceholder("タグ名で検索").fill("存在しないタグ12345");
    await page.waitForTimeout(500);

    // Should show "検索結果がありません" message
    await expect(page.getByText("検索結果がありません")).toBeVisible();
  });

  test("new tag button navigates to creation page", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/tags");

    await page.getByRole("link", { name: "新規作成" }).click();

    await page.waitForURL("/admin/tags/new", { timeout: 10000 });
    await expect(
      page.getByRole("heading", { name: "新規タグ作成" }),
    ).toBeVisible();
  });

  test("tag rows are clickable and navigate to edit page", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/tags");
    await page.waitForLoadState("networkidle");

    // Click on TypeScript tag
    await page.getByText("TypeScript").first().click();

    // Should navigate to edit page
    await page.waitForURL(/\/admin\/tags\/.*\/edit/, { timeout: 10000 });
    await expect(page.getByRole("heading", { name: "タグ編集" })).toBeVisible();
  });

  test("displays tag logos", async ({ page }: TestArgs) => {
    await page.goto("/admin/tags");
    await page.waitForLoadState("networkidle");

    // At least some logos should be visible (depends on UI implementation)
    const logoCount = await page.locator("img").count();
    expect(logoCount).toBeGreaterThan(0);
  });
});

/**
 * Seed tag editing tests (non-destructive)
 * These tests modify seed tags but do NOT save, to preserve seed data
 */
test.describe("seed tag editing (non-destructive)", () => {
  test("can view TypeScript tag edit page", async ({ page }: TestArgs) => {
    await page.goto("/admin/tags");
    await page.waitForLoadState("networkidle");

    // Click TypeScript tag
    await page.getByText("TypeScript").first().click();

    await page.waitForURL(/\/admin\/tags\/.*\/edit/, { timeout: 10000 });

    // Verify data is loaded
    const nameInput = page.getByPlaceholder("例: Next.js");
    await expect(nameInput).toHaveValue("TypeScript");
  });

  test("can modify seed tag form without saving", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/tags");
    await page.waitForLoadState("networkidle");

    // Click React tag
    await page.getByText("React").first().click();

    await page.waitForURL(/\/admin\/tags\/.*\/edit/, { timeout: 10000 });

    // Modify name
    const nameInput = page.getByPlaceholder("例: Next.js");
    await nameInput.clear();
    await nameInput.fill("Modified React");

    // Verify input changed
    await expect(nameInput).toHaveValue("Modified React");

    // Cancel without saving
    await page.getByRole("button", { name: "キャンセル" }).click();

    // Verify we're back at tag list
    await page.waitForURL("/admin/tags", { timeout: 10000 });

    // Verify original name is still there
    await expect(page.getByText("React").first()).toBeVisible();
  });
});
