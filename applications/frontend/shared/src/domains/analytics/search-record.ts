import z from "zod";
import { AsyncResult, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  UnexpectedError,
  validate,
  ValidationError,
  validationError,
} from "@shared/aspects/error";
import { ok, err } from "@shared/aspects/result";
import { dateKeySchema, dateRangeSchema } from "./common";
import { contentTypeSchema } from "../search-token/reference";
import { tagIdentifierSchema } from "../attributes/tag";

export const searchKeywordSchema = z.string().min(1).brand("SearchKeyword");

export type SearchKeyword = z.infer<typeof searchKeywordSchema>;

export const searchRecordIdentifierSchema = z
  .ulid()
  .brand("SearchRecordIdentifier");

export type SearchRecordIdentifier = z.infer<
  typeof searchRecordIdentifierSchema
>;

export const searchRecordSchema = z
  .object({
    identifier: searchRecordIdentifierSchema,
    dateKey: dateKeySchema,
    keyword: searchKeywordSchema,
    resultCount: z.number().int().min(0),
    tags: z.array(tagIdentifierSchema).nullable(),
    contentType: contentTypeSchema.nullable(),
    createdAt: z.date(),
  })
  .brand("SearchRecord");

export type SearchRecord = z.infer<typeof searchRecordSchema>;

export type UnvalidatedSearchRecord = {
  identifier: string;
  dateKey: string;
  keyword: string;
  resultCount: number;
  tags: string[] | null;
  contentType: string | null;
  createdAt: Date;
};

export const validateSearchRecord = (
  candidate: UnvalidatedSearchRecord,
): Result<SearchRecord, ValidationError[]> =>
  validate(searchRecordSchema, candidate);

export const validateSearchRecordIdentifier = (
  identifier: string,
): Result<SearchRecordIdentifier, ValidationError> => {
  const result = searchRecordIdentifierSchema.safeParse(identifier);
  if (result.success) {
    return ok(result.data);
  }
  return err(
    validationError(
      "identifier",
      result.error.issues[0]?.message ?? "Invalid identifier",
    ),
  );
};

export const validateSearchKeyword = (
  keyword: string,
): Result<SearchKeyword, ValidationError> => {
  const result = searchKeywordSchema.safeParse(keyword);
  if (result.success) {
    return ok(result.data);
  }
  return err(
    validationError(
      "keyword",
      result.error.issues[0]?.message ?? "Invalid keyword",
    ),
  );
};

export const criteriaSchema = z
  .object({
    dateRange: dateRangeSchema.nullish(),
    keyword: searchKeywordSchema.nullish(),
    hasResults: z.boolean().nullish(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  dateRange?: { start: Date; end: Date } | null;
  keyword?: string | null;
  hasResults?: boolean | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export interface SearchRecordRepository {
  find: (
    identifier: SearchRecordIdentifier,
  ) => AsyncResult<
    SearchRecord,
    AggregateNotFoundError<"SearchRecord"> | UnexpectedError
  >;

  search: (criteria: Criteria) => AsyncResult<SearchRecord[], UnexpectedError>;

  persist: (record: SearchRecord) => AsyncResult<void, UnexpectedError>;

  terminate: (
    identifier: SearchRecordIdentifier,
  ) => AsyncResult<
    void,
    AggregateNotFoundError<"SearchRecord"> | UnexpectedError
  >;
}
