/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";

vi.mock("@shared/components/molecules/link/navigable", () => ({
  NavigableLink: vi.fn(({ href, children }: { href: string; children: React.ReactNode }) =>
    ({ type: "a", props: { href, children } })
  ),
}));

vi.mock("@shared/components/atoms/icon/github", () => ({
  GithubIcon: vi.fn(() => null),
}));

vi.mock("@shared/components/atoms/icon/x-twitter", () => ({
  XTwitterIcon: vi.fn(() => null),
}));

vi.mock("@shared/components/atoms/icon/mail", () => ({
  MailIcon: vi.fn(() => null),
}));

vi.mock("@shared/components/atoms/icon/rss", () => ({
  RssIcon: vi.fn(() => null),
}));

vi.mock("@shared/config/presentation/route", () => ({
  Routes: {
    page: {
      articles: { index: "/articles" },
      memos: { index: "/memos" },
      series: { index: "/series" },
    },
  },
}));

vi.mock("./index.module.css", () => ({}));

describe("FooterPresenter", () => {
  it("RSSフィードリンクが含まれる", async () => {
    const { FooterPresenter } = await import(
      "@shared/components/organisms/footer/index.presenter"
    );

    const result = FooterPresenter({
      mailAddress: null,
      externalServices: new Map(),
    });

    const json = JSON.stringify(result);
    expect(json).toContain("/feed.xml");
  });
});
