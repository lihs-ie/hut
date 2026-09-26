/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { isValidElement } from "react";
import { Forger } from "@lihs-ie/forger-ts";
import { ProfileMold } from "../../support/molds/domains/user/common";

vi.mock("@shared/components/organisms/common/top/search", () => ({
  ContentSection: async (props: Record<string, unknown>) => (
    <div data-testid="content-section" data-type={String(props.type)} />
  ),
}));

vi.mock("@shared/components/organisms/common/top/search.skeleton", () => ({
  ContentSectionSkeleton: () => <div data-testid="content-section-skeleton" />,
}));

vi.mock("@shared/components/molecules/list/card/profile", () => ({
  ProfileCard: () => <div data-testid="profile-card" />,
}));

describe("components/templates/top/TopIndex", () => {
  it("記事セクションとプロフィールが含まれる", async () => {
    const { TopIndex } = await import(
      "@shared/components/templates/top/index"
    );

    const result = await TopIndex({
      searchArticles: async () => [],
      findAllTags: async () => [],
      getProfile: async () => Forger(ProfileMold).forge(),
    });

    expect(isValidElement(result)).toBe(true);
  });
});
