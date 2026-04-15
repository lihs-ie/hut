/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";

import { LogoutButton } from "@shared/components/molecules/button/logout";

describe("components/molecules/button/LogoutButton", () => {
  it("ログアウトボタンが accessible name 'ログアウト' を持つ", () => {
    render(<LogoutButton logout={vi.fn()} />);
    expect(
      screen.getByRole("button", { name: "ログアウト" }),
    ).toBeInTheDocument();
  });

  it("button 要素の type は submit である", () => {
    render(<LogoutButton logout={vi.fn()} />);
    expect(screen.getByRole("button", { name: "ログアウト" })).toHaveAttribute(
      "type",
      "submit",
    );
  });
});
