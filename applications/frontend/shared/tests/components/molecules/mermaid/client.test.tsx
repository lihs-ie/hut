/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, waitFor } from "@testing-library/react";
import { MermaidClient } from "@shared/components/molecules/mermaid/client";

vi.mock("next-themes", () => ({
  useTheme: () => ({ resolvedTheme: "light" }),
}));

vi.mock("mermaid", () => ({
  default: {
    initialize: vi.fn(),
    render: vi.fn(),
  },
}));

const getMermaidRenderMock = async () => {
  const mermaidModule = await import("mermaid");
  return vi.mocked(mermaidModule.default.render);
};

beforeEach(() => {
  vi.clearAllMocks();
});

describe("MermaidClient - sanitize", () => {
  it("scriptタグを含むsvgをレンダリング後DOMにscript要素が存在しない", async () => {
    const maliciousSvg = `<svg xmlns="http://www.w3.org/2000/svg"><script>alert('xss')</script><rect width="10" height="10"/></svg>`;
    const renderMock = await getMermaidRenderMock();
    renderMock.mockResolvedValue({
      svg: maliciousSvg,
      diagramType: "flowchart",
      bindFunctions: undefined,
    });

    const { container } = render(<MermaidClient code="graph TD; A-->B" />);

    await waitFor(() => {
      expect(container.querySelector("svg")).not.toBeNull();
    });

    expect(container.querySelector("script")).toBeNull();
  });

  it("iframeタグを含むsvgをレンダリング後DOMにiframe要素が存在しない", async () => {
    const maliciousSvg = `<svg xmlns="http://www.w3.org/2000/svg"><foreignObject width="100" height="100"><iframe src="javascript:alert('xss')"></iframe></foreignObject></svg>`;
    const renderMock = await getMermaidRenderMock();
    renderMock.mockResolvedValue({
      svg: maliciousSvg,
      diagramType: "flowchart",
      bindFunctions: undefined,
    });

    const { container } = render(<MermaidClient code="graph TD; A-->B" />);

    await waitFor(() => {
      expect(container.querySelector("svg")).not.toBeNull();
    });

    expect(container.querySelector("iframe")).toBeNull();
  });

  it("onerror属性を含むsvgをレンダリング後DOMにonerror属性が存在しない", async () => {
    const maliciousSvg = `<svg xmlns="http://www.w3.org/2000/svg"><image href="x" onerror="alert('xss')"/></svg>`;
    const renderMock = await getMermaidRenderMock();
    renderMock.mockResolvedValue({
      svg: maliciousSvg,
      diagramType: "flowchart",
      bindFunctions: undefined,
    });

    const { container } = render(<MermaidClient code="graph TD; A-->B" />);

    await waitFor(() => {
      expect(container.querySelector("svg")).not.toBeNull();
    });

    expect(container.querySelectorAll("[onerror]").length).toBe(0);
  });

  it("onload属性を含むsvgをレンダリング後DOMにonload属性が存在しない", async () => {
    const maliciousSvg = `<svg xmlns="http://www.w3.org/2000/svg" onload="alert('xss')"><rect width="10" height="10"/></svg>`;
    const renderMock = await getMermaidRenderMock();
    renderMock.mockResolvedValue({
      svg: maliciousSvg,
      diagramType: "flowchart",
      bindFunctions: undefined,
    });

    const { container } = render(<MermaidClient code="graph TD; A-->B" />);

    await waitFor(() => {
      expect(container.querySelector("svg")).not.toBeNull();
    });

    expect(container.querySelectorAll("[onload]").length).toBe(0);
  });

  it("onclick属性を含むsvgをレンダリング後DOMにonclick属性が存在しない", async () => {
    const maliciousSvg = `<svg xmlns="http://www.w3.org/2000/svg"><rect width="10" height="10" onclick="alert('xss')"/></svg>`;
    const renderMock = await getMermaidRenderMock();
    renderMock.mockResolvedValue({
      svg: maliciousSvg,
      diagramType: "flowchart",
      bindFunctions: undefined,
    });

    const { container } = render(<MermaidClient code="graph TD; A-->B" />);

    await waitFor(() => {
      expect(container.querySelector("svg")).not.toBeNull();
    });

    expect(container.querySelectorAll("[onclick]").length).toBe(0);
  });

  it("javascript:URIを含むhref属性はサニタイズによって除去される", async () => {
    const maliciousSvg = `<svg xmlns="http://www.w3.org/2000/svg"><a href="javascript:alert('xss')"><rect width="10" height="10"/></a></svg>`;
    const renderMock = await getMermaidRenderMock();
    renderMock.mockResolvedValue({
      svg: maliciousSvg,
      diagramType: "flowchart",
      bindFunctions: undefined,
    });

    const { container } = render(<MermaidClient code="graph TD; A-->B" />);

    await waitFor(() => {
      expect(container.querySelector("svg")).not.toBeNull();
    });

    const anchors = container.querySelectorAll("a");
    anchors.forEach((anchor) => {
      const href = anchor.getAttribute("href") ?? "";
      expect(href.toLowerCase().startsWith("javascript:")).toBe(false);
    });
  });

  it("正常なsvgはサニタイズ後も保持される", async () => {
    const normalSvg = `<svg xmlns="http://www.w3.org/2000/svg"><circle cx="50" cy="50" r="50"/></svg>`;
    const renderMock = await getMermaidRenderMock();
    renderMock.mockResolvedValue({
      svg: normalSvg,
      diagramType: "flowchart",
      bindFunctions: undefined,
    });

    const { container } = render(<MermaidClient code="graph TD; A-->B" />);

    await waitFor(() => {
      expect(container.querySelector("svg")).not.toBeNull();
      expect(container.querySelector("circle")).not.toBeNull();
    });
  });
});
