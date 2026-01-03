import z from "zod";
import { tagSchema } from "../common/tag";
import { AsyncResult, err, ok, Result } from "@/aspects/result";
import {
  AggregateNotFoundError,
  DuplicationError,
  ValidationError,
  validationErrors,
} from "@/aspects/error";

export const articleIdentifierSchema = z.ulid().brand("ArticleIdentifier");

export type ArticleIdentifier = z.infer<typeof articleIdentifierSchema>;

export const titleSchema = z
  .string()
  .min(1, { message: "Title must be at least 1 character long" })
  .max(100, { message: "Title must be at most 100 characters long" })
  .brand("Title");

export type ArticleTitle = z.infer<typeof titleSchema>;

export const contentSchema = z
  .string()
  .min(1, { message: "Content must be at least 1 character long" })
  .brand("Content");

export type ArticleContent = z.infer<typeof contentSchema>;

export const excerptSchema = z
  .string()
  .max(300, { message: "Excerpt must be at most 300 characters long" })
  .brand("Excerpt");

export type ArticleExcerpt = z.infer<typeof excerptSchema>;

export const slugSchema = z
  .string()
  .min(1, { message: "Slug must be at least 1 character long" })
  .max(100, { message: "Slug must be at most 100 characters long" })
  .regex(/^[a-z0-9]+(?:-[a-z0-9]+)*$/, {
    message:
      "Slug can only contain lowercase letters, numbers, and hyphens, and cannot start or end with a hyphen",
  })
  .brand("Slug");

export type ArticleSlug = z.infer<typeof slugSchema>;

export const Status = {
  DRAFT: "draft",
  PUBLISHED: "published",
} as const;

export const statusSchema = z.enum(Status).brand("Status");

export type Status = z.infer<typeof statusSchema>;

export const articleSchema = z
  .object({
    identifier: articleIdentifierSchema,
    title: titleSchema,
    content: contentSchema,
    excerpt: excerptSchema,
    slug: slugSchema,
    status: statusSchema,
    tags: z.array(tagSchema),
  })
  .brand("Article");

export type Article = z.infer<typeof articleSchema>;

export const toDraft = (article: Article): Article => ({
  ...article,
  status: Status.DRAFT as Status,
});

export const toPublished = (article: Article): Article => ({
  ...article,
  status: Status.PUBLISHED as Status,
});

export type UnvalidatedArticle = {
  identifier: string;
  title: string;
  content: string;
  excerpt: string;
  slug: string;
  status: string;
  tags: string[];
};

export const validateArticle = (
  candidate: UnvalidatedArticle
): Result<Article, ValidationError[]> => {
  const errors = validationErrors(articleSchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(articleSchema.parse(candidate));
};

export type ArticleError =
  | ValidationError
  | AggregateNotFoundError<"Article">
  | DuplicationError<"Article">;

export const criteriaSchema = z
  .object({
    slug: slugSchema.nullable(),
    status: statusSchema.nullable(),
    freeWord: z.string().min(1).max(100).nullable(),
    tags: z.array(tagSchema).nullable(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export interface ArticleRepository {
  persist: (
    article: Article
  ) => AsyncResult<
    void,
    AggregateNotFoundError<"Article"> | DuplicationError<"Article">
  >;
  find: (
    identifier: ArticleIdentifier
  ) => AsyncResult<Article, AggregateNotFoundError<"Article">>;
  search: (criteria: Criteria) => AsyncResult<Article[], ValidationError[]>;
  terminate: (
    identifier: ArticleIdentifier
  ) => AsyncResult<void, AggregateNotFoundError<"Article">>;
}
