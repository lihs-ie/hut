/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import React from "react";
import { extractStandaloneUrl } from "@shared/components/global/markdown-url";

describe("components/global/extractStandaloneUrl", () => {
  it("文字列のHTTP URLを検出する", () => {
    expect(extractStandaloneUrl("https://example.com")).toBe(
      "https://example.com"
    );
  });

  it("文字列のHTTP URLを検出する（httpスキーム）", () => {
    expect(extractStandaloneUrl("http://example.com")).toBe(
      "http://example.com"
    );
  });

  it("前後に空白がある文字列URLをトリムして検出する", () => {
    expect(extractStandaloneUrl("  https://example.com  ")).toBe(
      "https://example.com"
    );
  });

  it("URL以外の文字列に対してnullを返す", () => {
    expect(extractStandaloneUrl("通常のテキスト")).toBeNull();
  });

  it("URLを含む混合テキストに対してnullを返す", () => {
    expect(
      extractStandaloneUrl("テキスト https://example.com テキスト")
    ).toBeNull();
  });

  it("複数の子要素がある場合にnullを返す", () => {
    const children = [
      "テキスト",
      React.createElement("strong", null, "太字"),
    ];
    expect(extractStandaloneUrl(children)).toBeNull();
  });

  it("aタグのURLとテキストが一致する場合にURLを返す", () => {
    const anchor = React.createElement(
      "a",
      { href: "https://example.com" },
      "https://example.com"
    );
    expect(extractStandaloneUrl(anchor)).toBe("https://example.com");
  });

  it("aタグのURLとテキストが異なる場合にnullを返す", () => {
    const anchor = React.createElement(
      "a",
      { href: "https://example.com" },
      "リンクテキスト"
    );
    expect(extractStandaloneUrl(anchor)).toBeNull();
  });

  it("aタグのhrefがURL形式でない場合にnullを返す", () => {
    const anchor = React.createElement(
      "a",
      { href: "not-a-url" },
      "not-a-url"
    );
    expect(extractStandaloneUrl(anchor)).toBeNull();
  });

  it("空の子要素に対してnullを返す", () => {
    expect(extractStandaloneUrl(null)).toBeNull();
  });
});
