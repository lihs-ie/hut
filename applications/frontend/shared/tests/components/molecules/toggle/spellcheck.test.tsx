import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { SpellcheckToggle } from "@shared/components/molecules/toggle/spellcheck";

describe("components/molecules/toggle/SpellcheckToggle", () => {
  it("デフォルトラベルが表示される", () => {
    render(
      <SpellcheckToggle checked={false} onChange={() => {}} />,
    );
    expect(screen.getByText("スペルチェック")).toBeDefined();
  });

  it("カスタムラベルが表示される", () => {
    render(
      <SpellcheckToggle
        checked={false}
        onChange={() => {}}
        label="Spell Check"
      />,
    );
    expect(screen.getByText("Spell Check")).toBeDefined();
  });

  it("checked状態が反映される", () => {
    const { container } = render(
      <SpellcheckToggle checked={true} onChange={() => {}} />,
    );
    const input = container.querySelector(
      "input[type='checkbox']",
    ) as HTMLInputElement;
    expect(input.checked).toBe(true);
  });

  it("onChange がトグル時に呼ばれる", () => {
    const handleChange = vi.fn();
    const { container } = render(
      <SpellcheckToggle checked={false} onChange={handleChange} />,
    );
    const input = container.querySelector(
      "input[type='checkbox']",
    ) as HTMLInputElement;
    fireEvent.click(input);
    expect(handleChange).toHaveBeenCalledWith(true);
  });
});
