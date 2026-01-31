import z from "zod";
import { timelineSchema } from "../common";
import { ArticleIdentifier, articleIdentifierSchema } from "../articles";
import { MemoIdentifier, memoIdentifierSchema } from "../memo";
import { SeriesIdentifier, seriesIdentifierSchema } from "../series";
import { AsyncResult, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  UnexpectedError,
  validate,
  ValidationError,
} from "@shared/aspects/error";
import { tagIdentifierSchema } from "../attributes/tag";

export const contentTypeSchema = z
  .enum(["all", "article", "memo", "series"])
  .brand("ContentType");

export type ContentType = z.infer<typeof contentTypeSchema>;

export const ContentType = {
  ALL: contentTypeSchema.parse("all"),
  ARTICLE: contentTypeSchema.parse("article"),
  MEMO: contentTypeSchema.parse("memo"),
  SERIES: contentTypeSchema.parse("series"),
} as const;

export const sortSchema = z.enum(["latest", "newest", "oldest"]).brand("Sort");

export type Sort = z.infer<typeof sortSchema>;

export const Sort = {
  LATEST: sortSchema.parse("latest"),
  NEWEST: sortSchema.parse("newest"),
  OLDEST: sortSchema.parse("oldest"),
} as const;

export const orderSchema = z.enum(["asc", "desc"]).brand("Order");

export type Order = z.infer<typeof orderSchema>;

export const Order = {
  ASC: orderSchema.parse("asc"),
  DESC: orderSchema.parse("desc"),
} as const;

export const criteriaSchema = z
  .object({
    freeWord: z.string().min(1).max(100).nullable(),
    tags: z.array(tagIdentifierSchema).nullable(),
    type: contentTypeSchema.nullable(),
    sortBy: sortSchema.nullable(),
    order: orderSchema.nullable(),
  })
  .brand("SearchQuery");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  freeWord: string | null;
  tags: string[] | null;
  type: string | null;
  sortBy: string | null;
  order: string | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export const titleSchema = z.string().min(1).max(100).brand("SearchIndexTitle");

export type SearchIndexTitle = z.infer<typeof titleSchema>;

export const excerptSchema = z
  .string()
  .min(1)
  .max(300)
  .brand("SearchIndexExcerpt");

export type SearchIndexExcerpt = z.infer<typeof excerptSchema>;

export const slugSchema = z.string().min(1).max(100).brand("SearchIndexSlug");

export type SearchIndexSlug = z.infer<typeof slugSchema>;

export const searchIndexIdentifierSchema = z
  .ulid()
  .brand("SearchIndexIdentifier");

export type SearchIndexIdentifier = z.infer<typeof searchIndexIdentifierSchema>;

export const searchIndexSchema = z
  .object({
    identifier: searchIndexIdentifierSchema,
    type: contentTypeSchema,
    title: titleSchema,
    excerpt: excerptSchema,
    tags: z.array(tagIdentifierSchema),
    reference: articleIdentifierSchema
      .or(memoIdentifierSchema)
      .or(seriesIdentifierSchema),
    timeline: timelineSchema,
  })
  .refine((data) => {
    if (data.type === ContentType.ARTICLE) {
      return articleIdentifierSchema.safeParse(data.reference).success;
    }

    if (data.type === ContentType.MEMO) {
      return memoIdentifierSchema.safeParse(data.reference).success;
    }

    if (data.type === ContentType.SERIES) {
      return seriesIdentifierSchema.safeParse(data.reference).success;
    }

    return false;
  })
  .brand("SearchIndex");

export type UnvalidatedSearchIndex = {
  identifier: string;
  type: string;
  title: string;
  excerpt: string;
  tags: string[];
  reference: string;
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validateSearchIndex = (
  candidate: UnvalidatedSearchIndex,
): Result<SearchIndex, ValidationError[]> =>
  validate(searchIndexSchema, candidate);

export type SearchIndex = z.infer<typeof searchIndexSchema>;

export type SearchIndexError =
  | AggregateNotFoundError<"SearchIndex">
  | UnexpectedError;

export interface SearchIndexRepository {
  search: (criteria: Criteria) => AsyncResult<SearchIndex[], SearchIndexError>;
}

export const extractReferences = (
  indices: SearchIndex[],
): [ArticleIdentifier[], MemoIdentifier[], SeriesIdentifier[]] => {
  const articleIds: ArticleIdentifier[] = [];
  const memoIds: MemoIdentifier[] = [];
  const seriesIds: SeriesIdentifier[] = [];

  for (const index of indices) {
    switch (index.type) {
      case ContentType.ARTICLE:
        articleIds.push(index.reference as ArticleIdentifier);
        break;
      case ContentType.MEMO:
        memoIds.push(index.reference as MemoIdentifier);
        break;
      case ContentType.SERIES:
        seriesIds.push(index.reference as SeriesIdentifier);
        break;
    }
  }

  return [articleIds, memoIds, seriesIds];
};
