import z from "zod";
import { Result } from "@shared/aspects/result";
import { validate, ValidationError } from "@shared/aspects/error";
import { articleIdentifierSchema } from "../articles";
import { memoIdentifierSchema } from "../memo";
import { seriesIdentifierSchema } from "../series";

export const contentTypeSchema = z
  .enum(["article", "memo", "series"])
  .brand("ContentType");

export type ContentType = z.infer<typeof contentTypeSchema>;

export const ContentType = {
  ARTICLE: contentTypeSchema.parse("article"),
  MEMO: contentTypeSchema.parse("memo"),
  SERIES: contentTypeSchema.parse("series"),
} as const;

export const contentSchema = articleIdentifierSchema
  .or(memoIdentifierSchema)
  .or(seriesIdentifierSchema);

export type ContentSchema = z.infer<typeof contentSchema>;

export const searchReferenceIdentifierSchema = z.object({
  type: contentTypeSchema,
  content: contentSchema,
});

export type SearchReferenceIdentifier = z.infer<
  typeof searchReferenceIdentifierSchema
>;

export const equalSearchReferenceIdentifier = (
  left: SearchReferenceIdentifier,
  right: SearchReferenceIdentifier,
): boolean => {
  return left.type === right.type && left.content === right.content;
};

export const searchReferenceSchema = z
  .object({
    identifier: searchReferenceIdentifierSchema,
    score: z.number().min(0),
    updatedAt: z.date(),
  })
  .brand("SearchReference");

export type SearchReference = z.infer<typeof searchReferenceSchema>;

export type UnvalidatedSearchReference = {
  identifier: {
    type: string;
    content: string;
  };
  score: number;
  updatedAt: Date;
};

export const validateSearchReference = (
  candidate: UnvalidatedSearchReference,
): Result<SearchReference, ValidationError[]> =>
  validate(searchReferenceSchema, candidate);
