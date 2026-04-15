/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";

import { LogoutButton } from "@shared/components/molecules/button/logout";

describe("components/molecules/button/LogoutButton", () => {
  it("ログアウトボタンが accessible name 'ログアウト' を持つ", () => {
    render(<LogoutButton logout={vi.fn()} />);
    const button = screen.getByRole("button", { name: "ログアウト" });
    expect(button).not.toBeNull();
  });

  it("button 要素の type は submit である", () => {
    render(<LogoutButton logout={vi.fn()} />);
    const button = screen.getByRole("button", { name: "ログアウト" });
    expect(button.getAttribute("type")).toBe("submit");
  });
});
