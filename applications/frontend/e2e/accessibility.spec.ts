import AxeBuilder from "@axe-core/playwright";
import { expect, type Page, test } from "@playwright/test";

type TestArgs = {
  page: Page;
};

const targetPages = [
  { path: "/", name: "home" },
  { path: "/articles", name: "articles list" },
  { path: "/articles/typescript-type-safe-code", name: "article detail" },
  { path: "/search", name: "search" },
  { path: "/about", name: "about" },
  { path: "/series", name: "series list" },
  { path: "/series/rust-system-programming", name: "series detail" },
];

test.describe("accessibility", () => {
  for (const { path, name } of targetPages) {
    test(`${name} page (${path}) has no critical or serious a11y violations`, async ({
      page,
    }: TestArgs) => {
      const response = await page.goto(path, { waitUntil: "load" });

      expect(response?.status()).toBeLessThan(400);
      await expect(page.locator("main")).toBeVisible();

      const results = await new AxeBuilder({ page })
        .withTags(["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"])
        .analyze();

      const criticalViolations = results.violations.filter(
        (violation) =>
          violation.impact === "critical" || violation.impact === "serious",
      );

      if (criticalViolations.length > 0) {
        const report = criticalViolations.map((violation) => ({
          id: violation.id,
          impact: violation.impact,
          description: violation.description,
          helpUrl: violation.helpUrl,
          nodes: violation.nodes.map((node) => ({
            html: node.html,
            target: node.target,
            failureSummary: node.failureSummary,
          })),
        }));

        expect(
          criticalViolations,
          `${name} page has ${criticalViolations.length} violation(s):\n${JSON.stringify(report, null, 2)}`,
        ).toHaveLength(0);
      }
    });
  }
});
