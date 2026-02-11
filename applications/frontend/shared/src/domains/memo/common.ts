import z from "zod";
import { publishStatusSchema, Slug, slugSchema } from "../common";
import { timelineSchema } from "../common/date";
import { AsyncResult, err, ok, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  validate,
  validationError,
  ValidationError,
} from "@shared/aspects/error";
import { tagIdentifierSchema } from "../attributes/tag";

export const memoIdentifierSchema = z.ulid().brand("MemoIdentifier");

export type MemoIdentifier = z.infer<typeof memoIdentifierSchema>;

export const validateMemoIdentifier = (
  candidate: string,
): Result<MemoIdentifier, ValidationError> => {
  const result = memoIdentifierSchema.safeParse(candidate);

  if (!result.success) {
    return err(
      validationError(
        "MemoIdentifier",
        `Invalid MemoIdentifier format: ${candidate}`,
      ),
    );
  }

  return ok(result.data);
};

export const titleSchema = z
  .string()
  .min(1, { message: "Title must be at least 1 character long" })
  .max(100, { message: "Title must be at most 100 characters long" })
  .brand("Title");

export type MemoTitle = z.infer<typeof titleSchema>;

export const entrySchema = z
  .object({
    text: z
      .string()
      .min(1, { message: "Content text must be at least 1 character long" })
      .max(1000, {
        message: "Content text must be at most 1000 characters long",
      }),
    createdAt: z.date(),
  })
  .brand("Entry");

export type MemoEntry = z.infer<typeof entrySchema>;

export type MemoSlug = Slug;

export type UnvalidatedEntry = {
  text: string;
  createdAt: Date;
};

export const validateEntry = (
  candidate: UnvalidatedEntry,
): Result<MemoEntry, ValidationError[]> => validate(entrySchema, candidate);

export const memoSchema = z
  .object({
    identifier: memoIdentifierSchema,
    title: titleSchema,
    slug: slugSchema,
    entries: z.array(entrySchema),
    tags: z.array(tagIdentifierSchema),
    status: publishStatusSchema,
    timeline: timelineSchema,
  })
  .brand("Memo");

export type Memo = z.infer<typeof memoSchema>;

export const addEntry = (memo: Memo, entry: MemoEntry): Memo => {
  return {
    ...memo,
    entries: [...memo.entries, entry],
    timeline: {
      ...memo.timeline,
      updatedAt: entry.createdAt,
    },
  };
};

export type UnvalidatedMemo = {
  identifier: string;
  title: string;
  slug: string;
  entries: UnvalidatedEntry[];
  tags: string[];
  status: string;
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export type MemoError =
  | ValidationError
  | AggregateNotFoundError<"Memo">
  | DuplicationError<"Memo">;

export const validateMemo = (
  candidate: UnvalidatedMemo,
): Result<Memo, ValidationError[]> => validate(memoSchema, candidate);

export const criteriaSchema = z
  .object({
    tags: z.array(tagIdentifierSchema).nullable(),
    freeWord: z.string().min(1).max(100).nullable(),
    status: publishStatusSchema.nullable(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  tags: string[] | null;
  freeWord: string | null;
  status: string | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export const memoSnapshotSchema = z
  .object({
    identifier: memoIdentifierSchema,
    title: titleSchema,
    slug: slugSchema,
    entries: z.array(entrySchema),
    tags: z.array(tagIdentifierSchema),
    status: publishStatusSchema,
    timeline: timelineSchema,
  })
  .brand("MemoSnapshot");

export type MemoSnapshot = z.infer<typeof memoSnapshotSchema>;

export const toSnapshot = (memo: Memo): MemoSnapshot =>
  ({
    identifier: memo.identifier,
    title: memo.title,
    slug: memo.slug,
    entries: memo.entries,
    tags: memo.tags,
    status: memo.status,
    timeline: memo.timeline,
  }) as MemoSnapshot;

export interface MemoRepository {
  persist: (
    memo: Memo,
  ) => AsyncResult<
    void,
    AggregateNotFoundError<"Memo"> | DuplicationError<"Memo"> | UnexpectedError
  >;
  find: (
    identifier: MemoIdentifier,
  ) => AsyncResult<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>;
  findBySlug: (
    slug: MemoSlug,
  ) => AsyncResult<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>;
  ofIdentifiers: (
    identifiers: MemoIdentifier[],
    throwOnMissing?: boolean,
  ) => AsyncResult<Memo[], UnexpectedError | AggregateNotFoundError<"Memo">>;
  search: (criteria: Criteria) => AsyncResult<Memo[], UnexpectedError>;
  terminate: (
    identifier: MemoIdentifier,
  ) => AsyncResult<void, AggregateNotFoundError<"Memo"> | UnexpectedError>;
}
