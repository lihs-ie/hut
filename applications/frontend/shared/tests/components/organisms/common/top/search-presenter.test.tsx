/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import {
  ContentSectionPresenter,
  type Props,
} from "@shared/components/organisms/common/top/search.presenter";
import { ContentType } from "@shared/domains/search-token";

const createProps = (overrides: Partial<Props> = {}): Props => ({
  title: "ARTICLE",
  type: ContentType.ARTICLE,
  content: [],
  ...overrides,
});

describe("components/organisms/common/top/ContentSectionPresenter", () => {
  describe("空状態表示", () => {
    it("コンテンツが0件の場合、空状態メッセージが表示される", () => {
      render(<ContentSectionPresenter {...createProps()} />);

      expect(
        screen.getByText("まだコンテンツがありません")
      ).toBeInTheDocument();
    });

    it("コンテンツが0件の場合、廃止された説明文は表示されない", () => {
      render(<ContentSectionPresenter {...createProps()} />);

      expect(
        screen.queryByText(/コンテンツが公開される/)
      ).not.toBeInTheDocument();
    });
  });

  describe("コンテンツ表示", () => {
    it("コンテンツがある場合、一覧が表示される", () => {
      const content = [
        {
          slug: "test-article" as Props["content"][number]["slug"],
          type: ContentType.ARTICLE,
          title: "テスト記事",
          date: new Date("2026-01-01"),
          tagNames: [],
        },
      ];

      render(
        <ContentSectionPresenter
          {...createProps({ content })}
        />
      );

      expect(screen.getByText("テスト記事")).toBeInTheDocument();
    });

    it("コンテンツがある場合、空状態メッセージは表示されない", () => {
      const content = [
        {
          slug: "test-article" as Props["content"][number]["slug"],
          type: ContentType.ARTICLE,
          title: "テスト記事",
          date: new Date("2026-01-01"),
          tagNames: [],
        },
      ];

      render(
        <ContentSectionPresenter
          {...createProps({ content })}
        />
      );

      expect(
        screen.queryByText("まだコンテンツがありません")
      ).not.toBeInTheDocument();
    });
  });
});
