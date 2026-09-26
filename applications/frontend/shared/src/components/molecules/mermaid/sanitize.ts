import DOMPurify from "dompurify";

const FOREIGN_OBJECT_HTML_TAGS = [
  "foreignObject",
  "div",
  "span",
  "p",
  "br",
  "ul",
  "li",
  "ol",
  "h1",
  "h2",
  "h3",
  "h4",
  "h5",
  "h6",
  "a",
  "strong",
  "em",
  "code",
  "pre",
  "table",
  "tr",
  "td",
  "th",
  "tbody",
  "thead",
] as const;

const FORBIDDEN_TAGS = [
  "script",
  "style",
  "iframe",
  "object",
  "embed",
  "form",
  "input",
  "textarea",
  "select",
  "button",
  "template",
  "audio",
  "video",
  "link",
  "meta",
  "base",
] as const;

export const sanitizeMermaidSvg = (svg: string): string => {
  return DOMPurify.sanitize(svg, {
    USE_PROFILES: { svg: true, svgFilters: true },
    ADD_TAGS: [...FOREIGN_OBJECT_HTML_TAGS],
    FORBID_TAGS: [...FORBIDDEN_TAGS],
    FORBID_ATTR: ["onerror", "onload", "onclick", "onmouseover", "onfocus", "onblur"],
    HTML_INTEGRATION_POINTS: { foreignobject: true },
  });
};
