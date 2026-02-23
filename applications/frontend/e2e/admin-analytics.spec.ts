import { expect, type Locator, type Page, test } from "@playwright/test";

const ANALYTICS_URL = "/admin/analytics";
const LOAD_TIMEOUT = 30_000;

const navigateWithSingleRetry = async (page: Page, url: string) => {
  try {
    await page.goto(url, { waitUntil: "load" });
  } catch {
    await page.goto(url, { waitUntil: "load" });
  }
};

const getChartPanel = (page: Page, headingName: string) =>
  page
    .getByRole("heading", { name: headingName, level: 3 })
    .locator("../..");

const expectChartVisible = async (page: Page, headingName: string) => {
  const panel = getChartPanel(page, headingName);
  const svg = panel.locator("svg.recharts-surface").first();
  await expect(svg).toBeVisible({ timeout: LOAD_TIMEOUT });
};

const expectHeadingVisible = async (page: Page, headingName: string) => {
  await expect(
    page.getByRole("heading", { name: headingName, level: 3 }),
  ).toBeVisible({ timeout: LOAD_TIMEOUT });
};

const ensureChartRendered = async (panel: Locator): Promise<void> => {
  const svg = panel.locator("svg.recharts-surface").first();
  await expect(svg).toBeAttached({ timeout: LOAD_TIMEOUT });
  await svg.scrollIntoViewIfNeeded();
  await panel.page().waitForTimeout(500);
};

const hoverBarAndGetTooltipText = async (
  panel: Locator,
  barIndex: number,
): Promise<string> => {
  await ensureChartRendered(panel);
  const bars = panel.locator(
    "svg.recharts-surface .recharts-bar-rectangle path",
  );
  await expect(bars.first()).toBeVisible({ timeout: LOAD_TIMEOUT });
  await bars.nth(barIndex).hover({ force: true });

  const tooltip = panel.locator(".recharts-tooltip-wrapper");
  await expect(tooltip).toBeVisible({ timeout: LOAD_TIMEOUT });
  return (await tooltip.textContent()) ?? "";
};

const getSummaryCardLocator = (page: Page, title: string): Locator =>
  page.getByText(title, { exact: true }).first().locator("../..");

const getSummaryCardValue = async (
  page: Page,
  title: string,
): Promise<string> => {
  const card = getSummaryCardLocator(page, title);
  await expect(card).toBeVisible({ timeout: LOAD_TIMEOUT });
  const valueElement = card.locator("div").filter({ hasText: /^[\d:]+$/ });
  const text = await valueElement.first().textContent();
  return text ?? "";
};

const expectSummaryCardVisible = async (page: Page, title: string) => {
  const card = getSummaryCardLocator(page, title);
  await expect(card).toBeVisible({ timeout: LOAD_TIMEOUT });
  await expect(card.getByText("前期間比")).toBeVisible({
    timeout: LOAD_TIMEOUT,
  });
};

test.describe.serial("analytics navigation", () => {
  test.setTimeout(60_000);

  test("sidebar contains analytics navigation link", async ({ page }: { page: Page }) => {
    await navigateWithSingleRetry(page, "/admin/articles");

    await expect(page.getByRole("link", { name: "統計" })).toBeVisible({
      timeout: LOAD_TIMEOUT,
    });
  });

  test("analytics link navigates to /admin/analytics", async ({ page }: { page: Page }) => {
    await navigateWithSingleRetry(page, "/admin/articles");

    const link = page.getByRole("link", { name: "統計" });
    await expect(link).toBeVisible({ timeout: LOAD_TIMEOUT });
    await link.click();

    await page.waitForURL(ANALYTICS_URL, { timeout: LOAD_TIMEOUT });
    expect(page.url()).toContain(ANALYTICS_URL);
  });
});

test.describe("analytics dashboard", () => {
  test.describe.configure({ mode: "serial" });
  test.setTimeout(120_000);

  test.beforeEach(async ({ page }: { page: Page }) => {
    await navigateWithSingleRetry(page, ANALYTICS_URL);
    await page.waitForLoadState("networkidle");
  });

  test.describe("header and period selector", () => {
    test("page title is displayed", async ({ page }: { page: Page }) => {
      await expect(
        page.getByRole("heading", { name: "統計ダッシュボード", level: 1 }),
      ).toBeVisible({ timeout: LOAD_TIMEOUT });
    });

    test("period selection navigation is displayed", async ({ page }: { page: Page }) => {
      await expect(
        page.getByRole("navigation", { name: "期間選択" }),
      ).toBeVisible({ timeout: LOAD_TIMEOUT });
    });

    test("four period buttons are displayed", async ({ page }: { page: Page }) => {
      const navigation = page.getByRole("navigation", { name: "期間選択" });

      await expect(navigation).toBeVisible({ timeout: LOAD_TIMEOUT });

      const buttons = navigation.getByRole("button");

      await expect(buttons).toHaveCount(4, { timeout: LOAD_TIMEOUT });
      await expect(buttons.nth(0)).toHaveText("過去7日");
      await expect(buttons.nth(1)).toHaveText("過去30日");
      await expect(buttons.nth(2)).toHaveText("過去90日");
      await expect(buttons.nth(3)).toHaveText("全期間");
    });

    test("30 days button is active by default", async ({ page }: { page: Page }) => {
      const navigation = page.getByRole("navigation", { name: "期間選択" });

      await expect(navigation).toBeVisible({ timeout: LOAD_TIMEOUT });

      const thirtyDaysButton = navigation.getByRole("button", {
        name: "過去30日",
      });

      await expect(thirtyDaysButton).toHaveAttribute(
        "aria-pressed",
        "true",
        {
          timeout: LOAD_TIMEOUT,
        },
      );
    });

    test("clicking 7 days button updates URL", async ({ page }: { page: Page }) => {
      const navigation = page.getByRole("navigation", { name: "期間選択" });

      await expect(navigation).toBeVisible({ timeout: LOAD_TIMEOUT });
      await navigation.getByRole("button", { name: "過去7日" }).click();

      await expect(page).toHaveURL(/period=7d/, { timeout: LOAD_TIMEOUT });
    });

    test("clicking 90 days button updates URL", async ({ page }: { page: Page }) => {
      const navigation = page.getByRole("navigation", { name: "期間選択" });

      await expect(navigation).toBeVisible({ timeout: LOAD_TIMEOUT });
      await navigation.getByRole("button", { name: "過去90日" }).click();

      await expect(page).toHaveURL(/period=90d/, { timeout: LOAD_TIMEOUT });
    });

    test("clicking all period button updates URL", async ({ page }: { page: Page }) => {
      const navigation = page.getByRole("navigation", { name: "期間選択" });

      await expect(navigation).toBeVisible({ timeout: LOAD_TIMEOUT });
      await navigation.getByRole("button", { name: "全期間" }).click();

      await expect(page).toHaveURL(/period=all/, { timeout: LOAD_TIMEOUT });
    });
  });

  test.describe("summary cards", () => {
    test("four summary cards are displayed", async ({ page }: { page: Page }) => {
      const expectedTitles = [
        "総PV",
        "ユニークビジター",
        "平均滞在時間",
        "検索数",
      ];

      for (const title of expectedTitles) {
        await expect(
          page.getByText(title, { exact: true }).first(),
        ).toBeVisible({
          timeout: LOAD_TIMEOUT,
        });
      }
    });

    test("total page views card displays title and value", async ({ page }: { page: Page }) => {
      await expectSummaryCardVisible(page, "総PV");
    });

    test("unique visitors card displays title and value", async ({ page }: { page: Page }) => {
      await expectSummaryCardVisible(page, "ユニークビジター");
    });

    test("average dwell time card displays title and value", async ({ page }: { page: Page }) => {
      await expectSummaryCardVisible(page, "平均滞在時間");
    });

    test("search count card displays title and value", async ({ page }: { page: Page }) => {
      await expectSummaryCardVisible(page, "検索数");
    });
  });

  test.describe("page view trend section", () => {
    test("page view trend chart is displayed", async ({ page }: { page: Page }) => {
      await expectChartVisible(page, "PV推移");
    });

    test("content type comparison chart is displayed", async ({ page }: { page: Page }) => {
      await expectChartVisible(page, "コンテンツタイプ別PV");
    });
  });

  test.describe("content ranking section", () => {
    test("content page view ranking table is displayed", async ({ page }: { page: Page }) => {
      await expectHeadingVisible(page, "コンテンツPVランキング");
    });
  });

  test.describe("access source section", () => {
    test("referrer analysis chart is displayed", async ({ page }: { page: Page }) => {
      await expectChartVisible(page, "リファラー分析");
    });

    test("device ratio chart is displayed", async ({ page }: { page: Page }) => {
      await expectChartVisible(page, "デバイス比率");
    });
  });

  test.describe("engagement section", () => {
    test("average dwell time ranking table is displayed", async ({ page }: { page: Page }) => {
      await expectHeadingVisible(page, "平均滞在時間ランキング");
    });

    test("scroll depth distribution chart is displayed", async ({ page }: { page: Page }) => {
      await expectChartVisible(page, "スクロール深度分布");
    });
  });

  test.describe("search analytics section", () => {
    test("search count trend chart is displayed", async ({ page }: { page: Page }) => {
      await expectChartVisible(page, "検索回数推移");
    });

    test("search keyword ranking table is displayed", async ({ page }: { page: Page }) => {
      await expectHeadingVisible(page, "検索キーワード");
    });

    test("zero hit keywords table is displayed", async ({ page }: { page: Page }) => {
      await expectHeadingVisible(page, "ゼロヒットキーワード");
    });
  });

  test.describe("tag page view section", () => {
    test("tag page view chart is displayed", async ({ page }: { page: Page }) => {
      await expectChartVisible(page, "タグ別PV");
    });
  });

  test.describe("data validation", () => {
    test("summary cards render values in correct format", async ({ page }: { page: Page }) => {
      const cardSpecs = [
        { title: "総PV", pattern: /^\d+$/ },
        { title: "ユニークビジター", pattern: /^\d+$/ },
        { title: "平均滞在時間", pattern: /^\d+:\d{2}$/ },
        { title: "検索数", pattern: /^\d+$/ },
      ];

      for (const spec of cardSpecs) {
        const card = getSummaryCardLocator(page, spec.title);
        await expect(card).toBeVisible({ timeout: LOAD_TIMEOUT });

        const valueElement = card
          .locator("div")
          .filter({ hasText: spec.pattern });
        await expect(valueElement.first()).toBeVisible({
          timeout: LOAD_TIMEOUT,
        });
      }
    });

    test("unique visitors card displays positive count from seed data", async ({ page }: { page: Page }) => {
      const card = getSummaryCardLocator(page, "ユニークビジター");
      await expect(card).toBeVisible({ timeout: LOAD_TIMEOUT });

      const valueElement = card.locator("div").filter({ hasText: /^\d+$/ });
      await expect(valueElement.first()).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });
      const text = await valueElement.first().textContent();
      expect(Number(text)).toBeGreaterThan(0);
    });

    test("average dwell time card displays time format from seed data", async ({ page }: { page: Page }) => {
      const card = getSummaryCardLocator(page, "平均滞在時間");
      await expect(card).toBeVisible({ timeout: LOAD_TIMEOUT });

      const valueElement = card
        .locator("div")
        .filter({ hasText: /^\d+:\d{2}$/ });
      await expect(valueElement.first()).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });
      const text = await valueElement.first().textContent();
      const minutes = Number(text?.split(":")[0]);
      expect(minutes).toBeGreaterThanOrEqual(0);
    });

    test("search count card displays positive count from seed data", async ({ page }: { page: Page }) => {
      const card = getSummaryCardLocator(page, "検索数");
      await expect(card).toBeVisible({ timeout: LOAD_TIMEOUT });

      const valueElement = card.locator("div").filter({ hasText: /^\d+$/ });
      await expect(valueElement.first()).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });
      const text = await valueElement.first().textContent();
      expect(Number(text)).toBeGreaterThan(0);
    });

    test("summary cards display trend percentage with label", async ({ page }: { page: Page }) => {
      const summaryTitles = [
        "総PV",
        "ユニークビジター",
        "平均滞在時間",
        "検索数",
      ];

      for (const title of summaryTitles) {
        const card = getSummaryCardLocator(page, title);
        await expect(card).toBeVisible({ timeout: LOAD_TIMEOUT });

        const trendText = card.getByText(/%/);
        await expect(trendText).toBeVisible({ timeout: LOAD_TIMEOUT });
        await expect(card.getByText("前期間比")).toBeVisible({
          timeout: LOAD_TIMEOUT,
        });
      }
    });

    test("dwell time ranking displays items with time values", async ({ page }: { page: Page }) => {
      const heading = page.getByRole("heading", {
        name: "平均滞在時間ランキング",
        level: 3,
      });
      await expect(heading).toBeVisible({ timeout: LOAD_TIMEOUT });

      const section = heading.locator("../..");
      const timeValues = section.getByText(/^\d+:\d{2}$/);
      const count = await timeValues.count();
      expect(count).toBeGreaterThan(0);
    });

    test("search keyword ranking displays items with counts", async ({ page }: { page: Page }) => {
      const heading = page.getByRole("heading", {
        name: "検索キーワード",
        level: 3,
      });
      await expect(heading).toBeVisible({ timeout: LOAD_TIMEOUT });

      const section = heading.locator("../..");
      const countValues = section.getByText(/^\d+回$/);
      const count = await countValues.count();
      expect(count).toBeGreaterThan(0);
    });

    test("zero hit keywords ranking displays items with counts", async ({ page }: { page: Page }) => {
      const heading = page.getByRole("heading", {
        name: "ゼロヒットキーワード",
        level: 3,
      });
      await expect(heading).toBeVisible({ timeout: LOAD_TIMEOUT });

      const section = heading.locator("../..");
      const countValues = section.getByText(/^\d+回$/);
      const count = await countValues.count();
      expect(count).toBeGreaterThan(0);
    });

    test("search count trend line chart renders curve path", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "検索回数推移");
      await expect(panel.getByRole("heading", { name: "検索回数推移", level: 3 })).toBeVisible({ timeout: LOAD_TIMEOUT });

      const curve = panel.locator(
        "svg.recharts-surface .recharts-line .recharts-line-curve",
      );
      await expect(curve.first()).toBeVisible({ timeout: LOAD_TIMEOUT });
    });

    test("scroll depth bar chart renders data rectangles", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "スクロール深度分布");
      await expect(panel.getByRole("heading", { name: "スクロール深度分布", level: 3 })).toBeVisible({ timeout: LOAD_TIMEOUT });

      const rectangles = panel.locator(
        "svg.recharts-surface .recharts-bar-rectangle",
      );
      await expect(rectangles.first()).toBeVisible({ timeout: LOAD_TIMEOUT });
      const count = await rectangles.count();
      expect(count).toBeGreaterThan(0);
    });

    test("charts with axis render cartesian grid and ticks", async ({ page }: { page: Page }) => {
      const chartHeadings = [
        "PV推移",
        "コンテンツタイプ別PV",
        "リファラー分析",
        "スクロール深度分布",
        "検索回数推移",
        "タグ別PV",
      ];

      for (const headingName of chartHeadings) {
        const panel = getChartPanel(page, headingName);
        await expect(panel.getByRole("heading", { name: headingName, level: 3 })).toBeVisible({ timeout: LOAD_TIMEOUT });

        const grid = panel.locator(
          "svg.recharts-surface .recharts-cartesian-grid",
        );
        await expect(grid).toBeVisible({ timeout: LOAD_TIMEOUT });
      }
    });
  });

  test.describe("exact seed data values", () => {
    test("summary cards display exact values from seed data", async ({ page }: { page: Page }) => {
      const expectedCards = [
        { title: "総PV", value: "38" },
        { title: "ユニークビジター", value: "38" },
        { title: "平均滞在時間", value: "2:16" },
        { title: "検索数", value: "15" },
      ];

      for (const expected of expectedCards) {
        const cardValue = await getSummaryCardValue(page, expected.title);
        expect(cardValue).toBe(expected.value);
      }
    });

    test("summary cards display +100% trend from seed data", async ({ page }: { page: Page }) => {
      const summaryTitles = [
        "総PV",
        "ユニークビジター",
        "平均滞在時間",
        "検索数",
      ];

      for (const title of summaryTitles) {
        const card = getSummaryCardLocator(page, title);
        await expect(card).toBeVisible({ timeout: LOAD_TIMEOUT });
        await expect(card.getByText("+100%")).toBeVisible({
          timeout: LOAD_TIMEOUT,
        });
      }
    });

    test("content type chart bar tooltips show correct values", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "コンテンツタイプ別PV");
      const firstBarTooltip = await hoverBarAndGetTooltipText(panel, 0);
      expect(firstBarTooltip).toContain("34");

      const secondBarTooltip = await hoverBarAndGetTooltipText(panel, 1);
      expect(secondBarTooltip).toContain("4");
    });

    test("content type chart renders exactly two bars", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "コンテンツタイプ別PV");
      await ensureChartRendered(panel);
      const bars = panel.locator(
        "svg.recharts-surface .recharts-bar-rectangle",
      );
      await expect(bars).toHaveCount(2, { timeout: LOAD_TIMEOUT });
    });

    test("referrer analysis chart renders four bars with correct values", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "リファラー分析");
      await ensureChartRendered(panel);

      const bars = panel.locator(
        "svg.recharts-surface .recharts-bar-rectangle",
      );
      await expect(bars.first()).toBeVisible({ timeout: LOAD_TIMEOUT });
      const barCount = await bars.count();
      expect(barCount).toBe(4);

      const expectedValues = ["17", "16", "3", "2"];
      const tooltipValues: string[] = [];

      for (let index = 0; index < barCount; index++) {
        const tooltipText = await hoverBarAndGetTooltipText(panel, index);
        tooltipValues.push(tooltipText);
      }

      for (const value of expectedValues) {
        const found = tooltipValues.some((text) => text.includes(value));
        expect(found).toBe(true);
      }
    });

    test("scroll depth chart renders four bars", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "スクロール深度分布");
      await ensureChartRendered(panel);
      const bars = panel.locator(
        "svg.recharts-surface .recharts-bar-rectangle",
      );
      await expect(bars).toHaveCount(4, { timeout: LOAD_TIMEOUT });
    });

    test("scroll depth chart last bar tooltip shows highest value", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "スクロール深度分布");
      const tooltipText = await hoverBarAndGetTooltipText(panel, 3);

      expect(tooltipText).toContain("7");
    });

    test("device ratio chart displays correct legend items", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "デバイス比率");
      const legend = panel.locator(".recharts-legend-wrapper");
      await expect(legend).toBeVisible({ timeout: LOAD_TIMEOUT });

      const legendText = await legend.textContent();
      expect(legendText).toContain("desktop");
      expect(legendText).toContain("mobile");
      expect(legendText).toContain("tablet");
    });

    test("search keyword ranking displays expected keywords and counts", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "検索キーワード");

      await expect(panel.getByText("TypeScript")).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });
      await expect(panel.getByText("4回")).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });
      await expect(panel.getByText("Next.js")).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });
      await expect(panel.getByText("2回")).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });
    });

    test("zero hit keywords ranking displays expected keywords", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "ゼロヒットキーワード");

      const expectedKeywords = ["GraphQL", "Rust入門", "Docker"];
      for (const keyword of expectedKeywords) {
        await expect(panel.getByText(keyword)).toBeVisible({
          timeout: LOAD_TIMEOUT,
        });
      }

      const oneTimeValues = panel.getByText("1回");
      const count = await oneTimeValues.count();
      expect(count).toBeGreaterThanOrEqual(3);
    });

    test("dwell time ranking displays values in correct order", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "平均滞在時間ランキング");
      await expect(panel).toBeVisible({ timeout: LOAD_TIMEOUT });

      const valueElements = panel.locator("span").filter({ hasText: /^\d+:\d{2}$/ });
      await expect(valueElements.first()).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });

      const expectedOrder = ["3:30", "2:03", "1:40", "1:23"];
      const actualCount = await valueElements.count();
      expect(actualCount).toBe(expectedOrder.length);

      for (let index = 0; index < expectedOrder.length; index++) {
        const text = await valueElements.nth(index).textContent();
        expect(text).toBe(expectedOrder[index]);
      }
    });

    test("content page view ranking displays values in correct order", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "コンテンツPVランキング");
      await expect(panel).toBeVisible({ timeout: LOAD_TIMEOUT });

      const valueElements = panel
        .locator("span")
        .filter({ hasText: /^\d+ PV$/ });
      await expect(valueElements.first()).toBeVisible({
        timeout: LOAD_TIMEOUT,
      });

      const expectedOrder = ["18 PV", "9 PV", "7 PV", "4 PV"];
      const actualCount = await valueElements.count();
      expect(actualCount).toBe(expectedOrder.length);

      for (let index = 0; index < expectedOrder.length; index++) {
        const text = await valueElements.nth(index).textContent();
        expect(text).toBe(expectedOrder[index]);
      }
    });
  });

  test.describe("chart tooltip interactions", () => {
    test("device ratio chart shows tooltip on sector hover", async ({ page }: { page: Page }) => {
      const panel = getChartPanel(page, "デバイス比率");

      await expect(async () => {
        const sectorPath = panel.locator(".recharts-pie-sector path").first();

        const position = await sectorPath.evaluate((element) => {
          const path = element as SVGPathElement;
          const length = path.getTotalLength();
          const svgPoint = path.getPointAtLength(length * 0.1);
          const ctm = path.getScreenCTM();
          if (!ctm) {
            throw new Error("getScreenCTM returned null");
          }
          const screenX = svgPoint.x * ctm.a + svgPoint.y * ctm.c + ctm.e;
          const screenY = svgPoint.x * ctm.b + svgPoint.y * ctm.d + ctm.f;
          const rect = element.getBoundingClientRect();
          return {
            x: Math.round(screenX - rect.left),
            y: Math.round(screenY - rect.top),
          };
        });

        await sectorPath.hover({ force: true, position });
        await expect(panel.locator(".recharts-tooltip-wrapper")).toBeVisible();
      }).toPass({ timeout: LOAD_TIMEOUT });
    });
  });
});
