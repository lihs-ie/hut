/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { ErrorModal } from "@shared/components/molecules/modal/error";

describe("components/molecules/modal/ErrorModal", () => {
  const defaultProps = {
    isOpen: true,
    onClose: vi.fn(),
    message: "エラーが発生しました",
  };

  beforeEach(() => {
    vi.clearAllMocks();
  });

  afterEach(() => {
    document.body.style.overflow = "";
  });

  describe("表示制御", () => {
    it("isOpenがtrueの場合はモーダルが表示される", () => {
      render(<ErrorModal {...defaultProps} />);

      expect(
        screen.getByRole("heading", { name: "エラーが発生しました" })
      ).toBeInTheDocument();
    });

    it("isOpenがfalseの場合はモーダルが表示されない", () => {
      render(<ErrorModal {...defaultProps} isOpen={false} />);

      expect(
        screen.queryByText("エラーが発生しました")
      ).not.toBeInTheDocument();
    });
  });

  describe("コンテンツ表示", () => {
    it("デフォルトのタイトルが表示される", () => {
      render(<ErrorModal {...defaultProps} />);

      expect(
        screen.getByRole("heading", { name: "エラーが発生しました" })
      ).toBeInTheDocument();
    });

    it("カスタムタイトルが表示される", () => {
      render(<ErrorModal {...defaultProps} title="カスタムエラー" />);

      expect(screen.getByText("カスタムエラー")).toBeInTheDocument();
    });

    it("メッセージが表示される", () => {
      render(<ErrorModal {...defaultProps} message="詳細なエラーメッセージ" />);

      expect(screen.getByText("詳細なエラーメッセージ")).toBeInTheDocument();
    });

    it("エラー詳細が表示される", () => {
      const details = [
        { field: "email", description: "メールアドレスが無効です" },
        { field: "password", description: "パスワードは8文字以上必要です" },
      ];

      render(<ErrorModal {...defaultProps} details={details} />);

      expect(screen.getByText("email:")).toBeInTheDocument();
      expect(screen.getByText("メールアドレスが無効です")).toBeInTheDocument();
      expect(screen.getByText("password:")).toBeInTheDocument();
      expect(
        screen.getByText("パスワードは8文字以上必要です")
      ).toBeInTheDocument();
    });

    it("fieldがない詳細でもdescriptionが表示される", () => {
      const details = [{ description: "一般的なエラーです" }];

      render(<ErrorModal {...defaultProps} details={details} />);

      expect(screen.getByText("一般的なエラーです")).toBeInTheDocument();
    });

    it("詳細が空配列の場合は詳細セクションが表示されない", () => {
      render(<ErrorModal {...defaultProps} details={[]} />);

      expect(screen.queryByRole("list")).not.toBeInTheDocument();
    });

    it("デフォルトの閉じるボタンテキストが表示される", () => {
      render(<ErrorModal {...defaultProps} />);

      expect(screen.getByText("閉じる")).toBeInTheDocument();
    });

    it("カスタムの閉じるボタンテキストが表示される", () => {
      render(<ErrorModal {...defaultProps} closeText="OK" />);

      expect(screen.getByText("OK")).toBeInTheDocument();
    });
  });

  describe("インタラクション", () => {
    it("閉じるボタンをクリックするとonCloseが呼ばれる", () => {
      const onClose = vi.fn();
      render(<ErrorModal {...defaultProps} onClose={onClose} />);

      fireEvent.click(screen.getByText("閉じる"));

      expect(onClose).toHaveBeenCalledTimes(1);
    });

    it("オーバーレイをクリックするとonCloseが呼ばれる", () => {
      const onClose = vi.fn();
      render(<ErrorModal {...defaultProps} onClose={onClose} />);

      const heading = screen.getByRole("heading", {
        name: "エラーが発生しました",
      });
      const overlay = heading.closest("div")?.parentElement;
      if (overlay) {
        fireEvent.click(overlay);
      }

      expect(onClose).toHaveBeenCalledTimes(1);
    });

    it("モーダル本体をクリックしてもonCloseは呼ばれない", () => {
      const onClose = vi.fn();
      render(<ErrorModal {...defaultProps} onClose={onClose} />);

      fireEvent.click(
        screen.getByRole("heading", { name: "エラーが発生しました" })
      );

      expect(onClose).not.toHaveBeenCalled();
    });

    it("Escapeキーを押すとonCloseが呼ばれる", () => {
      const onClose = vi.fn();
      render(<ErrorModal {...defaultProps} onClose={onClose} />);

      fireEvent.keyDown(document, { key: "Escape" });

      expect(onClose).toHaveBeenCalledTimes(1);
    });
  });

  describe("スクロール制御", () => {
    it("モーダルが開いているときはbodyのoverflowがhiddenになる", () => {
      render(<ErrorModal {...defaultProps} />);

      expect(document.body.style.overflow).toBe("hidden");
    });

    it("モーダルが閉じるとbodyのoverflowがunsetになる", () => {
      const { rerender } = render(<ErrorModal {...defaultProps} />);

      expect(document.body.style.overflow).toBe("hidden");

      rerender(<ErrorModal {...defaultProps} isOpen={false} />);

      expect(document.body.style.overflow).toBe("unset");
    });
  });

  describe("アクセシビリティ", () => {
    it("エラーアイコンが表示される", () => {
      render(<ErrorModal {...defaultProps} />);

      // SVGアイコンが存在することを確認
      const iconWrapper = document.querySelector('[class*="icon-wrapper"]');
      expect(iconWrapper).toBeInTheDocument();
    });
  });
});
