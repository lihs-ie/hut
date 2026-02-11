import { validate } from "@shared/aspects/error";
import { matter, MatterError } from "@shared/aspects/mdx";
import { Result } from "@shared/aspects/result";
import { excerptSchema, titleSchema } from "@shared/domains/articles";
import { tagIdentifierSchema } from "@shared/domains/attributes/tag";
import { slugSchema } from "@shared/domains/common";
import z from "zod";

const articleFrontMatter = z
  .object({
    title: titleSchema,
    excerpt: excerptSchema,
    slug: slugSchema,
    tags: z.array(tagIdentifierSchema),
  })
  .brand("ArticleFrontMatter");

type ArticleFrontMatter = z.infer<typeof articleFrontMatter>;

export const ARTICLE_FRONTMATTER_TEMPLATE = `---
title:
excerpt:
slug:
tags: []
---

`;

export const updateFrontmatterTitle = (
  content: string,
  newTitle: string
): string => {
  const frontmatterMatch = /^---\n([\s\S]+?)\n---/.exec(content);

  if (!frontmatterMatch) {
    return content;
  }

  const frontmatterContent = frontmatterMatch[1];
  const restContent = content.slice(frontmatterMatch[0].length);

  const updatedFrontmatter = frontmatterContent.replace(
    /^title:.*$/m,
    `title: ${newTitle}`
  );

  return `---\n${updatedFrontmatter}\n---${restContent}`;
};

export const extractFrontmatterTitle = (content: string): string | null => {
  const frontmatterMatch = /^---\n([\s\S]+?)\n---/.exec(content);

  if (!frontmatterMatch) {
    return null;
  }

  const titleMatch = /^title:[ \t]*(.*)$/m.exec(frontmatterMatch[1]);
  return titleMatch ? titleMatch[1].trim() : null;
};

export const extractFrontmatterSlug = (content: string): string | null => {
  const frontmatterMatch = /^---\n([\s\S]+?)\n---/.exec(content);

  if (!frontmatterMatch) {
    return null;
  }

  const slugMatch = /^slug:[ \t]*(.*)$/m.exec(frontmatterMatch[1]);
  return slugMatch ? slugMatch[1].trim() : null;
};

export const updateFrontmatterTags = (
  content: string,
  newTags: readonly string[],
): string => {
  const frontmatterMatch = /^---\n([\s\S]+?)\n---/.exec(content);

  if (!frontmatterMatch) {
    return content;
  }

  const frontmatterContent = frontmatterMatch[1];
  const restContent = content.slice(frontmatterMatch[0].length);

  const tagsArray = `[${newTags.join(", ")}]`;
  const updatedFrontmatter = frontmatterContent.replace(
    /^tags:.*$/m,
    `tags: ${tagsArray}`,
  );

  return `---\n${updatedFrontmatter}\n---${restContent}`;
};

export const extractFrontmatterTags = (content: string): string[] | null => {
  const frontmatterMatch = /^---\n([\s\S]+?)\n---/.exec(content);

  if (!frontmatterMatch) {
    return null;
  }

  const tagsMatch = /^tags:[ \t]*\[(.*)\]$/m.exec(frontmatterMatch[1]);
  if (!tagsMatch) {
    return null;
  }

  const tagsString = tagsMatch[1].trim();
  if (tagsString === "") {
    return [];
  }

  return tagsString.split(",").map((tag) => tag.trim());
};

export const stripFrontmatter = (content: string): string => {
  if (!content.startsWith("---\n")) {
    return content;
  }

  const endIndex = content.indexOf("\n---", 4);
  if (endIndex === -1) {
    return content;
  }

  return content.slice(endIndex + 4).replace(/^\n+/, "");
};

export const articleMatter = matter<{
  title: string;
  excerpt: string;
  slug: string;
  tags: string[];
}>()((frontMatter): Result<ArticleFrontMatter, MatterError> => {
  const result = validate<"ArticleFrontMatter", ArticleFrontMatter>(
    articleFrontMatter,
    frontMatter
  );

  return result.mapError(
    (errors): MatterError => ({
      message: `Invalid article front matter: ${JSON.stringify(errors, null, 2)}`,
    })
  );
});
