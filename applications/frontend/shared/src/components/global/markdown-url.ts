import React from "react";

const URL_PATTERN = /^https?:\/\/[^\s]+$/;

export const extractStandaloneUrl = (
  children: React.ReactNode
): string | null => {
  if (React.Children.count(children) !== 1) {
    return null;
  }

  const child = React.Children.toArray(children)[0];

  if (typeof child === "string") {
    const trimmed = child.trim();
    if (URL_PATTERN.test(trimmed)) {
      return trimmed;
    }
  }

  if (
    React.isValidElement<{ href?: string; children?: React.ReactNode }>(
      child
    ) &&
    child.type === "a"
  ) {
    const href = child.props.href ?? "";
    const linkText =
      typeof child.props.children === "string"
        ? child.props.children.trim()
        : "";
    if (URL_PATTERN.test(href) && href === linkText) {
      return href;
    }
  }

  return null;
};
