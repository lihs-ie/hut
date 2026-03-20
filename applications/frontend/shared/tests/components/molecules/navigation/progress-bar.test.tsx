/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render } from "@testing-library/react";

describe("components/molecules/navigation/TopProgressBar", () => {
  it("visible=true のとき progress bar が表示される", async () => {
    const { TopProgressBar } = await import(
      "@shared/components/molecules/navigation/progress-bar"
    );

    const { container } = render(<TopProgressBar visible={true} />);

    const bar = container.querySelector('[data-testid="top-progress-bar"]');
    expect(bar).toBeInTheDocument();
  });

  it("visible=false のとき progress bar が表示されない", async () => {
    const { TopProgressBar } = await import(
      "@shared/components/molecules/navigation/progress-bar"
    );

    const { container } = render(<TopProgressBar visible={false} />);

    const bar = container.querySelector('[data-testid="top-progress-bar"]');
    expect(bar).not.toBeInTheDocument();
  });

  it("completing=false のとき data-completing 属性が false になる", async () => {
    const { TopProgressBar } = await import(
      "@shared/components/molecules/navigation/progress-bar"
    );

    const { container } = render(
      <TopProgressBar visible={true} completing={false} />,
    );

    const bar = container.querySelector('[data-testid="top-progress-bar"]');
    expect(bar?.getAttribute("data-completing")).toBe("false");
  });

  it("completing=true のとき data-completing 属性が true になる", async () => {
    const { TopProgressBar } = await import(
      "@shared/components/molecules/navigation/progress-bar"
    );

    const { container } = render(
      <TopProgressBar visible={true} completing={true} />,
    );

    const bar = container.querySelector('[data-testid="top-progress-bar"]');
    expect(bar?.getAttribute("data-completing")).toBe("true");
  });
});
