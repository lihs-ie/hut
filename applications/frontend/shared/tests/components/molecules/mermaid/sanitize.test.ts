import { describe, it, expect } from "vitest";
import { sanitizeMermaidSvg } from "@shared/components/molecules/mermaid/sanitize";

describe("sanitizeMermaidSvg - Node環境（isomorphic-dompurify）", () => {
  it("Node環境でもforeignObject要素を保持する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <foreignObject x="0" y="0" width="100" height="50">
        <div xmlns="http://www.w3.org/1999/xhtml">開始</div>
      </foreignObject>
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).toContain("foreignObject");
    expect(result).toContain("開始");
  });

  it("Node環境でもscriptタグを除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <script>alert('xss')</script>
      <rect x="0" y="0" width="100" height="50" />
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("script");
    expect(result).not.toContain("alert");
  });
});

describe("sanitizeMermaidSvg", () => {
  it("foreignObject要素を保持する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <foreignObject x="0" y="0" width="100" height="50">
        <div xmlns="http://www.w3.org/1999/xhtml">開始</div>
      </foreignObject>
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).toContain("foreignObject");
    expect(result).toContain("開始");
  });

  it("foreignObject内のdiv要素を保持する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <foreignObject x="0" y="0" width="200" height="100">
        <div xmlns="http://www.w3.org/1999/xhtml">
          <span>条件分岐</span>
        </div>
      </foreignObject>
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).toContain("div");
    expect(result).toContain("条件分岐");
  });

  it("通常のSVG要素（rect、pathなど）を保持する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <rect x="0" y="0" width="100" height="50" />
      <path d="M 0 0 L 100 0" />
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).toContain("rect");
    expect(result).toContain("path");
  });

  it("scriptタグを除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <script>alert('xss')</script>
      <rect x="0" y="0" width="100" height="50" />
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("script");
    expect(result).not.toContain("alert");
  });
});
