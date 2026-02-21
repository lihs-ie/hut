import z from "zod";
import { AsyncResult, err, ok, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  validate,
  validationError,
  ValidationError,
} from "@shared/aspects/error";
import {
  PublishStatus,
  publishStatusSchema,
  Slug,
  slugSchema,
  timelineSchema,
} from "../common";
import { tagIdentifierSchema } from "../attributes/tag";
import { imageIdentifierSchema } from "../image/identifier";

export const articleIdentifierSchema = z.ulid().brand("ArticleIdentifier");

export type ArticleIdentifier = z.infer<typeof articleIdentifierSchema>;

export const validateArticleIdentifier = (
  candidate: string,
): Result<ArticleIdentifier, ValidationError> => {
  const result = articleIdentifierSchema.safeParse(candidate);

  if (result.success) {
    return ok(result.data);
  } else {
    return err(
      validationError(
        "ArticleIdentifier",
        `Invalid article identifier: ${candidate}`,
      ),
    );
  }
};

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

export type ArticleSlug = Slug;

export const articleSchema = z
  .object({
    identifier: articleIdentifierSchema,
    title: titleSchema,
    content: contentSchema,
    excerpt: excerptSchema,
    slug: slugSchema,
    status: publishStatusSchema,
    tags: z.array(tagIdentifierSchema),
    images: z.array(imageIdentifierSchema),
    timeline: timelineSchema,
  })
  .brand("Article");

export type Article = z.infer<typeof articleSchema>;

export const toDraft = (article: Article): Article => ({
  ...article,
  status: PublishStatus.DRAFT as PublishStatus,
});

export const toPublished = (article: Article): Article => ({
  ...article,
  status: PublishStatus.PUBLISHED as PublishStatus,
});

export type UnvalidatedArticle = {
  identifier: string;
  title: string;
  content: string;
  excerpt: string;
  slug: string;
  status: string;
  tags: string[];
  images: string[];
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validateArticle = (
  candidate: UnvalidatedArticle,
): Result<Article, ValidationError[]> => validate(articleSchema, candidate);

export type ArticleError =
  | ValidationError
  | AggregateNotFoundError<"Article">
  | DuplicationError<"Article">
  | UnexpectedError;

export const criteriaSchema = z
  .object({
    slug: slugSchema.nullish(),
    status: publishStatusSchema.nullish(),
    freeWord: z.string().min(1).max(100).nullish(),
    tags: z.array(tagIdentifierSchema).nullish(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export type UnvalidatedCriteria = {
  slug?: string | null;
  status?: string | null;
  freeWord?: string | null;
  tags?: string[] | null;
};

export const articleSnapshotSchema = z
  .object({
    identifier: articleIdentifierSchema,
    title: titleSchema,
    content: contentSchema,
    excerpt: excerptSchema,
    slug: slugSchema,
    status: publishStatusSchema,
    tags: z.array(tagIdentifierSchema),
    images: z.array(imageIdentifierSchema),
    timeline: timelineSchema,
  })
  .brand("ArticleSnapshot");

export type ArticleSnapshot = z.infer<typeof articleSnapshotSchema>;

export const toSnapshot = (article: Article): ArticleSnapshot =>
  ({
    identifier: article.identifier,
    title: article.title,
    content: article.content,
    excerpt: article.excerpt,
    slug: article.slug,
    status: article.status,
    tags: article.tags,
    images: article.images,
    timeline: article.timeline,
  }) as ArticleSnapshot;

export interface ArticleRepository {
  persist: (
    article: Article,
  ) => AsyncResult<void, DuplicationError<"Article"> | UnexpectedError>;
  find: (
    identifier: ArticleIdentifier,
  ) => AsyncResult<
    Article,
    AggregateNotFoundError<"Article"> | UnexpectedError
  >;
  findBySlug: (
    slug: ArticleSlug,
  ) => AsyncResult<
    Article,
    AggregateNotFoundError<"Article"> | UnexpectedError
  >;
  ofIdentifiers: (
    identifiers: ArticleIdentifier[],
    throwOnMissing?: boolean,
  ) => AsyncResult<
    Article[],
    UnexpectedError | AggregateNotFoundError<"Article">
  >;
  search: (criteria: Criteria) => AsyncResult<Article[], UnexpectedError>;
  terminate: (
    identifier: ArticleIdentifier,
  ) => AsyncResult<void, AggregateNotFoundError<"Article"> | UnexpectedError>;
}
