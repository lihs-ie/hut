/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { ContentType } from "@shared/domains/search-token";

vi.mock("next/link", () => ({
  default: (props: { href: string; children: React.ReactNode; className?: string; scroll?: boolean }) => (
    <a href={props.href} className={props.className}>{props.children}</a>
  ),
}));

vi.mock("@shared/components/atoms/badge/content-type", () => ({
  ContentTypeBadge: (props: { children: React.ReactNode; variant: string; className?: string }) => (
    <span data-variant={props.variant}>{props.children}</span>
  ),
}));

vi.mock("@shared/aspects/date", () => ({
  formatDate: (date: Date) => date.toISOString().slice(0, 10),
}));

describe("components/molecules/list/card/HomeContentCard", () => {
  it("Series タイプでバッジに「シリーズ」が表示される", async () => {
    const { HomeContentCard } = await import(
      "@shared/components/molecules/list/card/home-content"
    );

    render(
      <HomeContentCard
        slug={"test-series" as import("@shared/domains/common").Slug}
        type={ContentType.SERIES}
        title="テストシリーズ"
        date={new Date("2024-01-01")}
        tagNames={[]}
      />,
    );

    expect(screen.getByText("シリーズ")).toBeInTheDocument();
  });

  it("Series タイプで /series/[slug] へのリンクが生成される", async () => {
    const { HomeContentCard } = await import(
      "@shared/components/molecules/list/card/home-content"
    );

    const { container } = render(
      <HomeContentCard
        slug={"my-series" as import("@shared/domains/common").Slug}
        type={ContentType.SERIES}
        title="マイシリーズ"
        date={new Date("2024-01-01")}
        tagNames={[]}
      />,
    );

    const link = container.querySelector("a");
    expect(link?.getAttribute("href")).toBe("/series/my-series");
  });
});
