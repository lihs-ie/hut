/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render } from "@testing-library/react";
import { MermaidSvg } from "@shared/components/molecules/mermaid/svg";

describe("MermaidSvg", () => {
  describe("成功パス (fallback未指定)", () => {
    it("渡されたSVG文字列をそのまま描画する", () => {
      const svg =
        '<svg xmlns="http://www.w3.org/2000/svg"><rect width="10" height="10" /></svg>';

      const { container } = render(<MermaidSvg html={svg} />);

      expect(container.querySelector("svg")).not.toBeNull();
      expect(container.querySelector("rect")).not.toBeNull();
    });

    it("ラッパーdivにmermaid-svgクラスを付与する", () => {
      const svg = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';

      const { container } = render(<MermaidSvg html={svg} />);

      const wrapper = container.querySelector(".mermaid-svg");
      expect(wrapper).not.toBeNull();
      expect(wrapper?.tagName).toBe("DIV");
    });

    it("fallback=falseでも成功パスでレンダリングされる", () => {
      const svg = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';

      const { container } = render(<MermaidSvg html={svg} fallback={false} />);

      expect(container.querySelector("svg")).not.toBeNull();
      expect(container.querySelector("pre")).toBeNull();
    });
  });

  describe("fallbackパス (fallback=true) - XSSリグレッション", () => {
    it("fallback時はpreタグでレンダリングする", () => {
      const { container } = render(
        <MermaidSvg html="flowchart TD" fallback />,
      );

      expect(
        container.querySelector("pre.mermaid-svg.fallback"),
      ).not.toBeNull();
      expect(container.querySelector("svg")).toBeNull();
    });

    it("fallback時にscriptタグが実行可能要素として埋め込まれない", () => {
      const malicious = "<script>alert('xss')</script>";

      const { container } = render(
        <MermaidSvg html={malicious} fallback />,
      );

      expect(container.querySelector("script")).toBeNull();
      const code = container.querySelector("code");
      expect(code?.textContent).toBe(malicious);
    });

    it("fallback時にimg onerrorが実際のimg要素として生成されない", () => {
      const malicious = '<img src=x onerror="alert(1)" />';

      const { container } = render(
        <MermaidSvg html={malicious} fallback />,
      );

      expect(container.querySelector("img")).toBeNull();
      const code = container.querySelector("code");
      expect(code?.textContent).toBe(malicious);
    });

    it("fallback時にiframeが実際のiframe要素として生成されない", () => {
      const malicious = '<iframe src="https://evil.example.com"></iframe>';

      const { container } = render(
        <MermaidSvg html={malicious} fallback />,
      );

      expect(container.querySelector("iframe")).toBeNull();
      const code = container.querySelector("code");
      expect(code?.textContent).toBe(malicious);
    });

    it("fallback時に特殊文字がテキストコンテンツとして保持される", () => {
      const input = `a && b <c> "quoted" 'single'`;

      const { container } = render(<MermaidSvg html={input} fallback />);

      const code = container.querySelector("code");
      expect(code?.textContent).toBe(input);
    });

    it("fallback時のinnerHTMLでHTML特殊文字がエスケープされている", () => {
      const malicious = "<script>alert(1)</script>";

      const { container } = render(
        <MermaidSvg html={malicious} fallback />,
      );

      expect(container.innerHTML).toContain("&lt;script&gt;");
      expect(container.innerHTML).not.toContain("<script>alert(1)</script>");
    });
  });
});
