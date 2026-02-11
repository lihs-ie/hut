import z from "zod";
import { AsyncResult, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  validate,
  ValidationError,
} from "@shared/aspects/error";
import { timelineSchema } from "../common";
import { tagIdentifierSchema } from "../attributes/tag";
import {
  contentTypeSchema,
  equalSearchReferenceIdentifier,
  SearchReference,
  SearchReferenceIdentifier,
  searchReferenceSchema,
  UnvalidatedSearchReference,
} from "./reference";

export const searchTokenTypeSchema = z
  .enum(["tag", "ngram"])
  .brand("SearchTokenType");

export type SearchTokenType = z.infer<typeof searchTokenTypeSchema>;

export const SearchTokenType = {
  TAG: searchTokenTypeSchema.parse("tag"),
  NGRAM: searchTokenTypeSchema.parse("ngram"),
} as const;

export const searchTokenValueSchema = z
  .string()
  .min(1)
  .max(100)
  .brand("SearchTokenValue");

export type SearchTokenValue = z.infer<typeof searchTokenValueSchema>;

export const searchTokenIdentifierSchema = z
  .string()
  .regex(/^(tag|ngram):.+$/)
  .brand("SearchTokenIdentifier");

export type SearchTokenIdentifier = z.infer<typeof searchTokenIdentifierSchema>;

export const createSearchTokenIdentifier = (
  type: SearchTokenType,
  value: SearchTokenValue,
): SearchTokenIdentifier => {
  return searchTokenIdentifierSchema.parse(`${type}:${value}`);
};

export const parseSearchTokenIdentifier = (
  identifier: SearchTokenIdentifier,
): { type: SearchTokenType; value: SearchTokenValue } => {
  const [typeString, ...valueParts] = identifier.split(":");
  const value = valueParts.join(":");
  return {
    type: searchTokenTypeSchema.parse(typeString),
    value: searchTokenValueSchema.parse(value),
  };
};

export const searchTokenSchema = z
  .object({
    identifier: searchTokenIdentifierSchema,
    references: z.array(searchReferenceSchema),
    type: searchTokenTypeSchema,
    value: searchTokenValueSchema,
    timeline: timelineSchema,
  })
  .brand("SearchToken");

export type SearchToken = z.infer<typeof searchTokenSchema>;

export const updateReferences = (
  token: SearchToken,
  references: SearchReference[],
): SearchToken => {
  return {
    ...token,
    references,
    timeline: {
      createdAt: token.timeline.createdAt,
      updatedAt: new Date(),
    },
  };
};

export const removeReferences = (
  token: SearchToken,
  references: SearchReferenceIdentifier[],
): SearchToken => {
  return {
    ...token,
    references: token.references.filter(
      (reference) =>
        !references.some((candidate) =>
          equalSearchReferenceIdentifier(candidate, reference.identifier),
        ),
    ),
    timeline: {
      createdAt: token.timeline.createdAt,
      updatedAt: new Date(),
    },
  };
};

export const replaceReferences = (
  token: SearchToken,
  references: SearchReference[],
): SearchToken => {
  const referenceMap = new Map<string, SearchReference>();

  for (const reference of token.references) {
    const key = JSON.stringify(reference.identifier);
    referenceMap.set(key, reference);
  }

  for (const reference of references) {
    const key = JSON.stringify(reference.identifier);
    referenceMap.set(key, reference);
  }

  return {
    ...token,
    references: Array.from(referenceMap.values()),
    timeline: {
      createdAt: token.timeline.createdAt,
      updatedAt: new Date(),
    },
  };
};

export type UnvalidatedSearchToken = {
  identifier: string;
  references: UnvalidatedSearchReference[];
  type: string;
  value: string;
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validateSearchToken = (
  candidate: UnvalidatedSearchToken,
): Result<SearchToken, ValidationError[]> =>
  validate(searchTokenSchema, candidate);

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
    limit: z.number().min(1).max(100).nullable(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  freeWord: string | null;
  tags: string[] | null;
  type: string | null;
  sortBy: string | null;
  order: string | null;
  limit: number | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export interface SearchTokenRepository {
  persist: (
    token: SearchToken,
  ) => AsyncResult<void, UnexpectedError | DuplicationError<"SearchToken">>;
  find: (
    identifier: SearchTokenIdentifier,
  ) => AsyncResult<
    SearchToken,
    UnexpectedError | AggregateNotFoundError<"SearchToken">
  >;
  ofIdentifiers: (
    identifiers: SearchTokenIdentifier[],
    throwOnMissing?: boolean,
  ) => AsyncResult<
    SearchToken[],
    UnexpectedError | AggregateNotFoundError<"SearchToken">
  >;
  search: (criteria: Criteria) => AsyncResult<SearchToken[], UnexpectedError>;
  terminate: (
    identifier: SearchTokenIdentifier,
  ) => AsyncResult<
    void,
    UnexpectedError | AggregateNotFoundError<"SearchToken">
  >;
}
