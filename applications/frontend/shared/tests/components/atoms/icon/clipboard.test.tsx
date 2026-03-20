/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render } from "@testing-library/react";
import { ClipboardIcon } from "@shared/components/atoms/icon/clipboard";

describe("components/atoms/icon/ClipboardIcon", () => {
  it("span要素としてレンダリングされる", () => {
    const { container } = render(<ClipboardIcon />);
    const span = container.querySelector("span");

    expect(span).toBeInTheDocument();
  });

  it("role='img'属性を持つ", () => {
    const { container } = render(<ClipboardIcon />);
    const span = container.querySelector("span");

    expect(span).toHaveAttribute("role", "img");
  });

  it("aria-hidden='true'属性を持つ", () => {
    const { container } = render(<ClipboardIcon />);
    const span = container.querySelector("span");

    expect(span).toHaveAttribute("aria-hidden", "true");
  });

  it("カスタムclassNameを追加できる", () => {
    const { container } = render(<ClipboardIcon className="custom" />);
    const span = container.querySelector("span");

    expect(span?.className).toContain("custom");
  });

  it("classNameを指定しない場合でもcontainerクラスを持つ", () => {
    const { container } = render(<ClipboardIcon />);
    const span = container.querySelector("span");

    expect(span?.className).toContain("container");
  });
});
