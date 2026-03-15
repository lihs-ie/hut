/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { EditorToolbar } from "@shared/components/organisms/common/editor/toolbar";

describe("EditorToolbar", () => {
  it("太字ボタンが表示される", () => {
    const props = {
      onBold: vi.fn(),
      onItalic: vi.fn(),
      onLink: vi.fn(),
      onImage: vi.fn(),
    };

    render(<EditorToolbar {...props} />);

    expect(screen.getByRole("button", { name: /太字/ })).toBeInTheDocument();
  });

  it("斜体ボタンが表示される", () => {
    const props = {
      onBold: vi.fn(),
      onItalic: vi.fn(),
      onLink: vi.fn(),
      onImage: vi.fn(),
    };

    render(<EditorToolbar {...props} />);

    expect(screen.getByRole("button", { name: /斜体/ })).toBeInTheDocument();
  });

  it("リンクボタンが表示される", () => {
    const props = {
      onBold: vi.fn(),
      onItalic: vi.fn(),
      onLink: vi.fn(),
      onImage: vi.fn(),
    };

    render(<EditorToolbar {...props} />);

    expect(screen.getByRole("button", { name: /リンク/ })).toBeInTheDocument();
  });

  it("画像ボタンが表示される", () => {
    const props = {
      onBold: vi.fn(),
      onItalic: vi.fn(),
      onLink: vi.fn(),
      onImage: vi.fn(),
    };

    render(<EditorToolbar {...props} />);

    expect(screen.getByRole("button", { name: /画像/ })).toBeInTheDocument();
  });

  it("太字ボタンクリックでonBoldが呼ばれる", () => {
    const onBold = vi.fn();
    const props = {
      onBold,
      onItalic: vi.fn(),
      onLink: vi.fn(),
      onImage: vi.fn(),
    };

    render(<EditorToolbar {...props} />);
    fireEvent.click(screen.getByRole("button", { name: /太字/ }));

    expect(onBold).toHaveBeenCalledOnce();
  });

  it("斜体ボタンクリックでonItalicが呼ばれる", () => {
    const onItalic = vi.fn();
    const props = {
      onBold: vi.fn(),
      onItalic,
      onLink: vi.fn(),
      onImage: vi.fn(),
    };

    render(<EditorToolbar {...props} />);
    fireEvent.click(screen.getByRole("button", { name: /斜体/ }));

    expect(onItalic).toHaveBeenCalledOnce();
  });

  it("リンクボタンクリックでonLinkが呼ばれる", () => {
    const onLink = vi.fn();
    const props = {
      onBold: vi.fn(),
      onItalic: vi.fn(),
      onLink,
      onImage: vi.fn(),
    };

    render(<EditorToolbar {...props} />);
    fireEvent.click(screen.getByRole("button", { name: /リンク/ }));

    expect(onLink).toHaveBeenCalledOnce();
  });

  it("画像ボタンクリックでonImageが呼ばれる", () => {
    const onImage = vi.fn();
    const props = {
      onBold: vi.fn(),
      onItalic: vi.fn(),
      onLink: vi.fn(),
      onImage,
    };

    render(<EditorToolbar {...props} />);
    fireEvent.click(screen.getByRole("button", { name: /画像/ }));

    expect(onImage).toHaveBeenCalledOnce();
  });

  it("rootのclassNameにcontainerが含まれる", () => {
    const props = {
      onBold: vi.fn(),
      onItalic: vi.fn(),
      onLink: vi.fn(),
      onImage: vi.fn(),
    };

    const { container } = render(<EditorToolbar {...props} />);

    const rootElement = container.firstChild as HTMLElement;
    expect(rootElement.className).toMatch(/container/);
  });
});
