import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import React from "react";

const mockNextThemesProvider = vi.fn();

vi.mock("next-themes", () => ({
  ThemeProvider: (props: { children: React.ReactNode; attribute?: string; defaultTheme?: string; themes?: string[] }) => {
    mockNextThemesProvider(props);
    return React.createElement("div", { "data-testid": "next-themes-provider" }, props.children);
  },
}));

describe("ThemeProvider", () => {
  it("子要素をレンダリングする", async () => {
    const { ThemeProvider } = await import("@shared/components/molecules/theme/provider");
    render(
      React.createElement(ThemeProvider, null,
        React.createElement("div", { "data-testid": "child" }, "child content")
      )
    );

    expect(screen.getByTestId("child")).toBeInTheDocument();
    expect(screen.getByText("child content")).toBeInTheDocument();
  });

  it("attribute='class'でNextThemesProviderに渡す", async () => {
    const { ThemeProvider } = await import("@shared/components/molecules/theme/provider");
    render(
      React.createElement(ThemeProvider, null,
        React.createElement("span", null, "test")
      )
    );

    expect(mockNextThemesProvider).toHaveBeenCalledWith(
      expect.objectContaining({ attribute: "class" })
    );
  });

  it("defaultTheme='light'でNextThemesProviderに渡す", async () => {
    const { ThemeProvider } = await import("@shared/components/molecules/theme/provider");
    render(
      React.createElement(ThemeProvider, null,
        React.createElement("span", null, "test")
      )
    );

    expect(mockNextThemesProvider).toHaveBeenCalledWith(
      expect.objectContaining({ defaultTheme: "light" })
    );
  });
});
