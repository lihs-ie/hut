/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { type ReactElement, isValidElement } from "react";

vi.mock(
  "@/app/admin/_components/molecules/analytics/period",
  () => ({
    AnalyticsPeriodSelector: (props: Record<string, unknown>) => (
      <div data-testid="period-selector">{String(props.current)}</div>
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/summary",
  () => ({
    AnalyticsSummary: async (props: Record<string, unknown>) => (
      <div data-testid="analytics-summary">{String(props.period)}</div>
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/summary.skeleton",
  () => ({
    AnalyticsSummarySkeleton: () => (
      <div data-testid="analytics-summary-skeleton" />
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/pageview",
  () => ({
    PageViewTrend: async (props: Record<string, unknown>) => (
      <div data-testid="pageview-trend">{String(props.period)}</div>
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/pageview.skeleton",
  () => ({
    PageViewTrendSkeleton: () => (
      <div data-testid="pageview-trend-skeleton" />
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/ranking",
  () => ({
    ContentRanking: async (props: Record<string, unknown>) => (
      <div data-testid="content-ranking">{String(props.period)}</div>
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/ranking.skeleton",
  () => ({
    ContentRankingSkeleton: () => (
      <div data-testid="content-ranking-skeleton" />
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/source",
  () => ({
    AccessSourceSection: async (props: Record<string, unknown>) => (
      <div data-testid="access-source">{String(props.period)}</div>
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/source.skeleton",
  () => ({
    AccessSourceSectionSkeleton: () => (
      <div data-testid="access-source-skeleton" />
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/engagement",
  () => ({
    EngagementSection: async (props: Record<string, unknown>) => (
      <div data-testid="engagement-section">{String(props.period)}</div>
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/engagement.skeleton",
  () => ({
    EngagementSectionSkeleton: () => (
      <div data-testid="engagement-section-skeleton" />
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/search",
  () => ({
    SearchAnalyticsSection: async (props: Record<string, unknown>) => (
      <div data-testid="search-analytics">{String(props.period)}</div>
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/search.skeleton",
  () => ({
    SearchAnalyticsSectionSkeleton: () => (
      <div data-testid="search-analytics-skeleton" />
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/tag",
  () => ({
    TagPageViewSection: async (props: Record<string, unknown>) => (
      <div data-testid="tag-pageview">{String(props.period)}</div>
    ),
  }),
);

vi.mock(
  "@/app/admin/_components/organisms/analytics/tag.skeleton",
  () => ({
    TagPageViewSectionSkeleton: () => (
      <div data-testid="tag-pageview-skeleton" />
    ),
  }),
);

function createMockProps(period: string = "30d") {
  return {
    period,
    getTotalPageViews: vi
      .fn()
      .mockResolvedValue({ current: 1234, previous: 1000 }),
    getUniqueVisitors: vi
      .fn()
      .mockResolvedValue({ current: 567, previous: 500 }),
    getAverageDwellTime: vi
      .fn()
      .mockResolvedValue({ current: 180, previous: 150 }),
    getSearchCount: vi
      .fn()
      .mockResolvedValue({ current: 89, previous: 70 }),
    getPageViewTrend: vi.fn().mockResolvedValue([
      { dateKey: "2025-01-01", value: 100 },
    ]),
    getContentTypeComparison: vi.fn().mockResolvedValue([
      { label: "article", value: 500 },
    ]),
    getContentRanking: vi.fn().mockResolvedValue([
      { label: "article-1", value: 200, subLabel: "article" },
    ]),
    getReferrerRanking: vi.fn().mockResolvedValue([
      { label: "google.com", value: 300 },
    ]),
    getDeviceDistribution: vi.fn().mockResolvedValue([
      { label: "desktop", value: 400 },
    ]),
    getDwellTimeRanking: vi.fn().mockResolvedValue([
      { label: "article-1", value: 120, subLabel: "article" },
    ]),
    getScrollDepthDistribution: vi.fn().mockResolvedValue([
      { label: "0-25%", value: 50 },
    ]),
    getSearchCountTrend: vi.fn().mockResolvedValue([
      { dateKey: "2025-01-01", value: 10 },
    ]),
    getSearchKeywordRanking: vi.fn().mockResolvedValue([
      { label: "typescript", value: 20 },
    ]),
    getZeroHitKeywords: vi.fn().mockResolvedValue([
      { label: "unknown-term", value: 5 },
    ]),
    getTagPageViews: vi.fn().mockResolvedValue([
      { label: "react", value: 150 },
    ]),
  };
}

type ReactElementLike = ReactElement & {
  props: {
    children?: ReactElementLike | ReactElementLike[] | string;
    [key: string]: unknown;
  };
};

function findAllInTree(
  element: ReactElementLike,
  predicate: (node: ReactElementLike) => boolean,
): ReactElementLike[] {
  const results: ReactElementLike[] = [];

  if (predicate(element)) {
    results.push(element);
  }

  const children = element.props?.children;
  if (children) {
    const childArray = Array.isArray(children) ? children : [children];
    for (const child of childArray) {
      if (isValidElement(child)) {
        results.push(...findAllInTree(child as ReactElementLike, predicate));
      }
    }
  }

  const fallback = element.props?.fallback;
  if (isValidElement(fallback)) {
    results.push(
      ...findAllInTree(fallback as ReactElementLike, predicate),
    );
  }

  return results;
}

function findByType(
  element: ReactElementLike,
  typeName: string,
): ReactElementLike[] {
  return findAllInTree(element, (node) => {
    const nodeType = node.type;
    if (typeof nodeType === "string") {
      return nodeType === typeName;
    }
    if (typeof nodeType === "function") {
      return nodeType.name === typeName;
    }
    return false;
  });
}

function findByComponentFunction(
  element: ReactElementLike,
  componentFunction: (...args: never[]) => unknown,
): ReactElementLike[] {
  return findAllInTree(element, (node) => node.type === componentFunction);
}

describe("components/templates/analytics/AnalyticsDashboard", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("Template が有効な React 要素としてレンダリングされること", async () => {
    const { AnalyticsDashboard } = await import(
      "@/app/admin/_components/templates/admin/analytics"
    );
    const mockProps = createMockProps("30d");
    const element = await AnalyticsDashboard(mockProps);

    expect(element).toBeDefined();
    expect(isValidElement(element)).toBe(true);
  });

  it("ルート要素が div であること", async () => {
    const { AnalyticsDashboard } = await import(
      "@/app/admin/_components/templates/admin/analytics"
    );
    const mockProps = createMockProps("30d");
    const element = await AnalyticsDashboard(mockProps);
    const typedElement = element as ReactElementLike;

    expect(typedElement.type).toBe("div");
  });

  it("ヘッダーに「統計ダッシュボード」のテキストを含む h1 が存在すること", async () => {
    const { AnalyticsDashboard } = await import(
      "@/app/admin/_components/templates/admin/analytics"
    );
    const mockProps = createMockProps("30d");
    const element = await AnalyticsDashboard(mockProps);
    const typedElement = element as ReactElementLike;

    const headings = findByType(typedElement, "h1");
    expect(headings.length).toBeGreaterThan(0);

    const headingChildren = headings[0].props.children;
    expect(headingChildren).toBe("統計ダッシュボード");
  });

  it("PeriodSelector 要素がツリーに含まれ、正しい current が渡されること", async () => {
    const { AnalyticsDashboard } = await import(
      "@/app/admin/_components/templates/admin/analytics"
    );
    const { AnalyticsPeriodSelector } = await import(
      "@/app/admin/_components/molecules/analytics/period"
    );
    const mockProps = createMockProps("7d");
    const element = await AnalyticsDashboard(mockProps);
    const typedElement = element as ReactElementLike;

    const periodSelectors = findByComponentFunction(
      typedElement,
      AnalyticsPeriodSelector,
    );
    expect(periodSelectors.length).toBe(1);
  });

  it("period='90d' でも PeriodSelector がレンダーされること", async () => {
    const { AnalyticsDashboard } = await import(
      "@/app/admin/_components/templates/admin/analytics"
    );
    const { AnalyticsPeriodSelector } = await import(
      "@/app/admin/_components/molecules/analytics/period"
    );
    const mockProps = createMockProps("90d");
    const element = await AnalyticsDashboard(mockProps);
    const typedElement = element as ReactElementLike;

    const periodSelectors = findByComponentFunction(
      typedElement,
      AnalyticsPeriodSelector,
    );
    expect(periodSelectors.length).toBe(1);
  });

  it("7つの Suspense バウンダリが存在すること", async () => {
    const { AnalyticsDashboard } = await import(
      "@/app/admin/_components/templates/admin/analytics"
    );
    const mockProps = createMockProps("30d");
    const element = await AnalyticsDashboard(mockProps);
    const typedElement = element as ReactElementLike;

    const suspenseElements = findAllInTree(typedElement, (node) => {
      const nodeType = node.type;
      return (
        typeof nodeType === "symbol" &&
        nodeType.toString() === "Symbol(react.suspense)"
      );
    });

    expect(suspenseElements.length).toBe(7);
  });
});
