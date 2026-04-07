import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import React from "react";

const mockSetTheme = vi.fn();
let mockTheme = "light";

vi.mock("next-themes", () => ({
  useTheme: () => ({
    theme: mockTheme,
    setTheme: mockSetTheme,
  }),
}));

vi.mock("@shared/components/atoms/icon/sun", () => ({
  SunIcon: () => React.createElement("span", { "data-testid": "sun-icon" }),
}));

vi.mock("@shared/components/atoms/icon/moon", () => ({
  MoonIcon: () => React.createElement("span", { "data-testid": "moon-icon" }),
}));

describe("ThemeToggle", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockTheme = "light";
  });

  it("テーマがlightのとき月アイコンを表示する（darkへの切り替えを示す）", async () => {
    mockTheme = "light";
    const { ThemeToggle } = await import("@shared/components/molecules/toggle/theme");
    render(React.createElement(ThemeToggle));

    expect(screen.getByTestId("moon-icon")).toBeInTheDocument();
    expect(screen.queryByTestId("sun-icon")).not.toBeInTheDocument();
  });

  it("テーマがdarkのとき太陽アイコンを表示する（lightへの切り替えを示す）", async () => {
    mockTheme = "dark";
    const { ThemeToggle } = await import("@shared/components/molecules/toggle/theme");
    render(React.createElement(ThemeToggle));

    expect(screen.getByTestId("sun-icon")).toBeInTheDocument();
    expect(screen.queryByTestId("moon-icon")).not.toBeInTheDocument();
  });

  it("lightのときボタンをクリックするとdarkに切り替える", async () => {
    mockTheme = "light";
    const { ThemeToggle } = await import("@shared/components/molecules/toggle/theme");
    render(React.createElement(ThemeToggle));

    fireEvent.click(screen.getByRole("button"));

    expect(mockSetTheme).toHaveBeenCalledWith("dark");
  });

  it("darkのときボタンをクリックするとlightに切り替える", async () => {
    mockTheme = "dark";
    const { ThemeToggle } = await import("@shared/components/molecules/toggle/theme");
    render(React.createElement(ThemeToggle));

    fireEvent.click(screen.getByRole("button"));

    expect(mockSetTheme).toHaveBeenCalledWith("light");
  });

  it("ThemeToggleはPropsを受け取らない", async () => {
    mockTheme = "light";
    const { ThemeToggle } = await import("@shared/components/molecules/toggle/theme");
    expect(() => render(React.createElement(ThemeToggle))).not.toThrow();
  });
});
