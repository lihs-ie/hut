import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

// Firestore Emulator REST API configuration
const FIRESTORE_EMULATOR_HOST = "http://localhost:8085";
const PROJECT_ID = "demo-hut";
const DATABASE_ID = "(default)";
const BASE_URL = `${FIRESTORE_EMULATOR_HOST}/v1/projects/${PROJECT_ID}/databases/${DATABASE_ID}/documents`;

/**
 * Get profile data from Firestore Emulator
 */
async function getProfileFromFirestore(): Promise<Record<string, unknown> | null> {
  const url = `${BASE_URL}/admin/admin`;
  const response = await fetch(url);

  if (!response.ok) {
    return null;
  }

  const data = await response.json();
  return data;
}

/**
 * Extract string value from Firestore response
 */
function extractStringValue(
  data: Record<string, unknown>,
  path: string[],
): string | null {
  let current: unknown = data;

  for (const key of path) {
    if (typeof current !== "object" || current === null) {
      return null;
    }
    current = (current as Record<string, unknown>)[key];
  }

  if (
    typeof current === "object" &&
    current !== null &&
    "stringValue" in current
  ) {
    return (current as { stringValue: string }).stringValue;
  }

  return null;
}

/**
 * Profile edit tests - require authentication
 * Uses serial mode to maintain state across tests
 */
test.describe.serial("profile edit", () => {
  test("profile edit page renders with heading", async ({ page }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Verify "プロフィール設定" heading is displayed
    await expect(
      page.getByRole("heading", { name: "プロフィール設定" }),
    ).toBeVisible();
  });

  test("avatar upload section is displayed with change button", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Verify avatar change button is displayed
    await expect(page.getByText("変更する")).toBeVisible();

    // Verify file input exists (hidden but present)
    const fileInput = page.locator('input[type="file"][accept="image/*"]');
    await expect(fileInput).toBeAttached();
  });

  test("avatar upload accepts image file", async ({ page }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Create a test image file
    const fileInput = page.locator('input[type="file"][accept="image/*"]');

    // Upload a test image (using a simple placeholder)
    await fileInput.setInputFiles({
      name: "test-avatar.png",
      mimeType: "image/png",
      buffer: Buffer.from(
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
        "base64",
      ),
    });

    // Verify no error occurred (page still functional)
    await expect(
      page.getByRole("heading", { name: "プロフィール設定" }),
    ).toBeVisible();

    // Note: We don't save, so data is not affected
  });

  test("display name input field exists and accepts input", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Find display name field by label
    const nameLabel = page.getByText("表示名", { exact: false });
    await expect(nameLabel).toBeVisible();

    // Find and fill the input (first text input after the avatar section)
    const nameInput = page
      .locator("form")
      .filter({ hasText: "表示名" })
      .locator('input[type="text"]')
      .first();
    await expect(nameInput).toBeVisible();

    // Clear and type new value
    await nameInput.fill("テスト表示名");
    await expect(nameInput).toHaveValue("テスト表示名");

    // Note: We don't save, so data is not affected
  });

  test("bio textarea exists and accepts input", async ({ page }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Find bio field by label
    const bioLabel = page.getByText("自己紹介");
    await expect(bioLabel).toBeVisible();

    // Find textarea with placeholder
    const bioTextarea = page.getByPlaceholder("自己紹介を入力");
    await expect(bioTextarea).toBeVisible();

    // Fill the textarea
    await bioTextarea.fill("これはテスト用の自己紹介文です。");
    await expect(bioTextarea).toHaveValue("これはテスト用の自己紹介文です。");

    // Note: We don't save, so data is not affected
  });

  test("GitHub username input exists and accepts input", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Find GitHub field by label
    const githubLabel = page.getByText("GitHubユーザーID");
    await expect(githubLabel).toBeVisible();

    // Find input in the GitHub field section
    const githubField = githubLabel.locator("..").locator('input[type="text"]');
    await expect(githubField).toBeVisible();

    // Clear existing value and type new value
    await githubField.clear();
    await githubField.pressSequentially("test-github-user");
    await expect(githubField).toHaveValue("test-github-user");

    // Note: We don't save, so data is not affected
  });

  test("X username input exists and accepts input", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Find X field by label
    const xLabel = page.getByText("XユーザーID");
    await expect(xLabel).toBeVisible();

    // Find input in the X field section
    const xField = xLabel.locator("..").locator('input[type="text"]');
    await expect(xField).toBeVisible();

    // Clear existing value and type new value
    await xField.clear();
    await xField.pressSequentially("test-x-user");
    await expect(xField).toHaveValue("test-x-user");

    // Note: We don't save, so data is not affected
  });

  test("tech stack section is displayed with add button", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Verify tech stack section header
    const techStackHeader = page.getByRole("heading", { name: "技術スタック" });
    await expect(techStackHeader).toBeVisible();

    // Verify add button exists in tech stack section
    const addButton = techStackHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await expect(addButton).toBeVisible();
  });

  test("tech stack add button creates new entry", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Wait for page to load
    await page.waitForLoadState("networkidle");

    // Count initial tech stack selects
    const initialSelectCount = await page
      .locator("select")
      .filter({ hasText: "選択してください" })
      .count();

    // Find and click add button in tech stack section
    const techStackHeader = page.getByRole("heading", { name: "技術スタック" });
    const addButton = techStackHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await addButton.click();

    // Verify a new tech stack entry was added
    const newSelectCount = await page
      .locator("select")
      .filter({ hasText: "選択してください" })
      .count();
    expect(newSelectCount).toBe(initialSelectCount + 1);

    // Note: We don't save, so data is not affected
  });

  test("tech stack entry can be edited", async ({ page }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    await page.waitForLoadState("networkidle");

    // Add a new tech stack entry first
    const techStackHeader = page.getByRole("heading", { name: "技術スタック" });
    const addButton = techStackHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await addButton.click();

    // Select a technology from dropdown
    const techSelect = page
      .locator("select")
      .filter({ hasText: "選択してください" })
      .first();
    await techSelect.selectOption("React");

    // Verify selection
    await expect(techSelect).toHaveValue("React");

    // Find and check "現在も使用中" checkbox
    const continueCheckbox = page.getByLabel("現在も使用中").first();
    await continueCheckbox.check();
    await expect(continueCheckbox).toBeChecked();

    // Note: We don't save, so data is not affected
  });

  test("career section is displayed with add button", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Verify career section header
    const careerHeader = page.getByRole("heading", { name: "経歴" });
    await expect(careerHeader).toBeVisible();

    // Verify add button exists in career section
    const addButton = careerHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await expect(addButton).toBeVisible();
  });

  test("career add button creates new entry", async ({ page }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    await page.waitForLoadState("networkidle");

    // Count initial career entries
    const initialCount = await page.getByPlaceholder("株式会社〇〇").count();

    // Find and click add button in career section
    const careerHeader = page.getByRole("heading", { name: "経歴" });
    const addButton = careerHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await addButton.click();

    // Verify a new career entry was added
    const newCount = await page.getByPlaceholder("株式会社〇〇").count();
    expect(newCount).toBe(initialCount + 1);

    // Note: We don't save, so data is not affected
  });

  test("career entry can be edited", async ({ page }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    await page.waitForLoadState("networkidle");

    // Find and click add button in career section
    const careerHeader = page.getByRole("heading", { name: "経歴" });
    const addButton = careerHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await addButton.click();

    // Fill company name
    const companyInput = page.getByPlaceholder("株式会社〇〇").first();
    await companyInput.clear();
    await companyInput.pressSequentially("テスト株式会社");
    await expect(companyInput).toHaveValue("テスト株式会社");

    // Fill role
    const roleInput = page.getByPlaceholder("Senior Developer").first();
    await roleInput.clear();
    await roleInput.pressSequentially("ソフトウェアエンジニア");
    await expect(roleInput).toHaveValue("ソフトウェアエンジニア");

    // Fill description
    const descriptionTextarea = page
      .getByPlaceholder("具体的な業務内容を記載")
      .first();
    await descriptionTextarea.clear();
    await descriptionTextarea.pressSequentially(
      "Webアプリケーションの開発を担当しました。",
    );
    await expect(descriptionTextarea).toHaveValue(
      "Webアプリケーションの開発を担当しました。",
    );

    // Note: We don't save, so data is not affected
  });

  test("tech stack entry can be removed", async ({ page }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    await page.waitForLoadState("networkidle");

    // Count initial tech stack entries
    const initialSelectCount = await page
      .locator("select")
      .filter({ hasText: "選択してください" })
      .count();

    // Add a new tech stack entry
    const techStackHeader = page.getByRole("heading", { name: "技術スタック" });
    const addButton = techStackHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await addButton.click();

    // Verify entry was added
    const newSelectCount = await page
      .locator("select")
      .filter({ hasText: "選択してください" })
      .count();
    expect(newSelectCount).toBe(initialSelectCount + 1);

    // Click the remove button (tech stack has aria-label="削除")
    const removeButton = page.getByRole("button", { name: "削除" }).first();
    await removeButton.click();

    // Verify entry was removed
    const afterRemoveCount = await page
      .locator("select")
      .filter({ hasText: "選択してください" })
      .count();
    expect(afterRemoveCount).toBe(initialSelectCount);

    // Note: We don't save, so data is not affected
  });

  test("update button is displayed and enabled", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    // Verify "更新する" button is displayed and enabled
    const updateButton = page.getByRole("button", { name: "更新する" });
    await expect(updateButton).toBeVisible();
    await expect(updateButton).toBeEnabled();
  });

  test("full form edit workflow (non-destructive)", async ({
    page,
  }: TestArgs) => {
    await page.goto("/admin/profile/edit");

    await page.waitForLoadState("networkidle");

    // Edit display name
    const nameInput = page
      .locator("form")
      .filter({ hasText: "表示名" })
      .locator('input[type="text"]')
      .first();
    await nameInput.clear();
    await nameInput.pressSequentially("統合テスト名");

    // Edit bio
    const bioTextarea = page.getByPlaceholder("自己紹介を入力");
    await bioTextarea.clear();
    await bioTextarea.pressSequentially("統合テストの自己紹介です。");

    // Edit GitHub username
    const githubLabel = page.getByText("GitHubユーザーID");
    const githubInput = githubLabel.locator("..").locator('input[type="text"]');
    await githubInput.clear();
    await githubInput.pressSequentially("integration-test");

    // Add tech stack
    const techStackHeader = page.getByRole("heading", { name: "技術スタック" });
    const techAddButton = techStackHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await techAddButton.click();

    const techSelect = page
      .locator("select")
      .filter({ hasText: "選択してください" })
      .first();
    await techSelect.selectOption("TypeScript");

    // Add career
    const careerHeader = page.getByRole("heading", { name: "経歴" });
    const careerAddButton = careerHeader
      .locator("..")
      .getByRole("button", { name: "追加" });
    await careerAddButton.click();

    const companyInput = page.getByPlaceholder("株式会社〇〇").first();
    await companyInput.clear();
    await companyInput.pressSequentially("統合テスト株式会社");

    // Verify all edits are reflected
    await expect(nameInput).toHaveValue("統合テスト名");
    await expect(bioTextarea).toHaveValue("統合テストの自己紹介です。");
    await expect(githubInput).toHaveValue("integration-test");
    await expect(techSelect).toHaveValue("TypeScript");
    await expect(companyInput).toHaveValue("統合テスト株式会社");

    // Verify update button is still available
    await expect(page.getByRole("button", { name: "更新する" })).toBeEnabled();

    // Note: We intentionally do NOT click the update button to keep the test non-destructive
  });
});

/**
 * Profile save tests - verify data persistence to Firestore
 * These tests actually save data and verify it in Firestore, then restore original data
 */
test.describe.serial("profile save with Firestore verification", () => {
  // Store original values to restore after test
  let originalName: string | null = null;
  let originalBio: string | null = null;

  test.beforeAll(async () => {
    // Get original profile data from Firestore before tests
    const profileData = await getProfileFromFirestore();
    if (profileData) {
      originalName = extractStringValue(profileData, [
        "fields",
        "profile",
        "mapValue",
        "fields",
        "name",
      ]);
      originalBio = extractStringValue(profileData, [
        "fields",
        "profile",
        "mapValue",
        "fields",
        "bio",
      ]);
    }
  });

  test("save profile and verify in Firestore", async ({ page }: TestArgs) => {
    // Collect console errors
    const consoleErrors: string[] = [];
    page.on("console", (msg) => {
      if (msg.type() === "error") {
        consoleErrors.push(msg.text());
      }
    });

    await page.goto("/admin/profile/edit");
    await page.waitForLoadState("networkidle");

    // Change display name to a unique test value
    const testName = `E2Eテスト_${Date.now()}`;
    const nameInput = page
      .locator("form")
      .filter({ hasText: "表示名" })
      .locator('input[type="text"]')
      .first();
    await nameInput.clear();
    await nameInput.pressSequentially(testName);

    // Click the update button to save
    const updateButton = page.getByRole("button", { name: "更新する" });
    await updateButton.click();

    // Wait for save operation to complete
    await page.waitForTimeout(3000);

    // Check if error modal appeared
    const errorModal = page.getByText("プロフィールの更新に失敗しました");
    const hasError = await errorModal.isVisible().catch(() => false);

    if (hasError) {
      // Get error details if available
      const errorDetails = await page.getByRole("dialog").textContent();
      console.log("Save error:", errorDetails);
      console.log("Console errors:", consoleErrors);
      throw new Error(`Profile save failed: ${errorDetails}`);
    }

    // Print any console errors for debugging
    if (consoleErrors.length > 0) {
      console.log("Console errors during save:", consoleErrors);
    }

    // Reload page to verify saved data persisted
    await page.reload();
    await page.waitForLoadState("networkidle");

    // Verify the saved name is displayed after reload
    const reloadedNameInput = page
      .locator("form")
      .filter({ hasText: "表示名" })
      .locator('input[type="text"]')
      .first();
    await expect(reloadedNameInput).toHaveValue(testName, { timeout: 10000 });

    // Also verify in Firestore
    const profileData = await getProfileFromFirestore();
    expect(profileData).not.toBeNull();

    const savedName = extractStringValue(profileData!, [
      "fields",
      "profile",
      "mapValue",
      "fields",
      "name",
    ]);
    expect(savedName).toBe(testName);
  });

  test("restore original profile data", async ({ page }: TestArgs) => {
    // Skip if we don't have original values
    if (!originalName) {
      console.log("Skipping restore - no original name stored");
      return;
    }

    await page.goto("/admin/profile/edit");
    await page.waitForLoadState("networkidle");

    // Restore original name
    const nameInput = page
      .locator("form")
      .filter({ hasText: "表示名" })
      .locator('input[type="text"]')
      .first();
    await nameInput.clear();
    await nameInput.pressSequentially(originalName);

    // Restore original bio if available
    if (originalBio) {
      const bioTextarea = page.getByPlaceholder("自己紹介を入力");
      await bioTextarea.clear();
      await bioTextarea.pressSequentially(originalBio);
    }

    // Save the restored data
    const updateButton = page.getByRole("button", { name: "更新する" });
    await updateButton.click();

    // Wait for save to complete
    await expect(updateButton).toHaveText("更新する", { timeout: 10000 });

    // Verify restoration in Firestore
    await page.waitForTimeout(1000);
    const profileData = await getProfileFromFirestore();
    expect(profileData).not.toBeNull();

    const restoredName = extractStringValue(profileData!, [
      "fields",
      "profile",
      "mapValue",
      "fields",
      "name",
    ]);
    expect(restoredName).toBe(originalName);
  });
});
