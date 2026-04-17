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

describe("sanitizeMermaidSvg - XSSリグレッション", () => {
  it("onerror属性を除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <image href="x" onerror="alert('xss')" />
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("onerror");
    expect(result).not.toContain("alert");
  });

  it("onload属性を除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg" onload="alert(1)">
      <rect width="10" height="10" />
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("onload");
  });

  it("onclick属性を除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <rect onclick="alert(1)" width="10" height="10" />
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("onclick");
  });

  it("javascript: URIのhref属性を無害化する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <a href="javascript:alert('xss')"><rect width="10" height="10" /></a>
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toMatch(/href=["']javascript:/i);
  });

  it("javascript: URIのxlink:href属性を無害化する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
      <use xlink:href="javascript:alert('xss')" />
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toMatch(/xlink:href=["']javascript:/i);
  });

  it("iframeタグを除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <foreignObject x="0" y="0" width="100" height="50">
        <iframe src="https://evil.example.com"></iframe>
      </foreignObject>
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("iframe");
    expect(result).not.toContain("evil.example.com");
  });

  it("objectタグとembedタグを除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <foreignObject x="0" y="0" width="100" height="50">
        <object data="https://evil.example.com/x.swf"></object>
        <embed src="https://evil.example.com/y.swf" />
      </foreignObject>
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("<object");
    expect(result).not.toContain("<embed");
  });

  it("foreignObject内のformとinputを除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <foreignObject x="0" y="0" width="200" height="100">
        <form action="https://evil.example.com">
          <input type="password" name="stolen" />
        </form>
      </foreignObject>
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("<form");
    expect(result).not.toContain("<input");
  });

  it("styleタグを除去する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <style>svg { background: url('javascript:alert(1)'); }</style>
      <rect width="10" height="10" />
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("<style");
  });

  it("data: URIのscript実行を無害化する", () => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg">
      <a href="data:text/html,<script>alert(1)</script>"><rect width="10" height="10" /></a>
    </svg>`;

    const result = sanitizeMermaidSvg(svg);

    expect(result).not.toContain("<script");
  });
});
