import z from "zod";
import { tagSchema } from "../common/tag";
import { timelineSchema } from "../common/date";
import { AsyncResult, err, ok, Result } from "@/aspects/result";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  ValidationError,
  validationErrors,
} from "@/aspects/error";

export const memoIdentifierSchema = z.ulid().brand("MemoIdentifier");

export type MemoIdentifier = z.infer<typeof memoIdentifierSchema>;

export const titleSchema = z
  .string()
  .min(1, { message: "Title must be at least 1 character long" })
  .max(100, { message: "Title must be at most 100 characters long" })
  .brand("Title");

export type MemoTitle = z.infer<typeof titleSchema>;

export const entrySchema = z.object({
  text: z
    .string()
    .min(1, { message: "Content text must be at least 1 character long" })
    .max(1000, {
      message: "Content text must be at most 1000 characters long",
    }),
  createdAt: z.date(),
});

export type MemoEntry = z.infer<typeof entrySchema>;

export type UnvalidatedEntry = {
  text: string;
  createdAt: Date;
};

export const validateEntry = (
  candidate: UnvalidatedEntry
): Result<MemoEntry, ValidationError[]> => {
  const errors = validationErrors(entrySchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(entrySchema.parse(candidate));
};

export const memoSchema = z
  .object({
    identifier: memoIdentifierSchema,
    title: titleSchema,
    entries: z.array(entrySchema),
    tags: z.array(tagSchema),
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
  entries: UnvalidatedEntry[];
  tags: string[];
};

export type MemoError =
  | ValidationError
  | AggregateNotFoundError<"Memo">
  | DuplicationError<"Memo">;

export const validateMemo = (
  candidate: UnvalidatedMemo
): Result<Memo, ValidationError[]> => {
  const errors = validationErrors(memoSchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(memoSchema.parse(candidate));
};

export const criteriaSchema = z
  .object({
    tag: tagSchema.nullable(),
    freeWord: z.string().min(1).max(100).nullable(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  tag: string | null;
  freeWord: string | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria
): Result<Criteria, ValidationError[]> => {
  const errors = validationErrors(criteriaSchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(criteriaSchema.parse(candidate));
};

export interface MemoRepository {
  persist: (
    memo: Memo
  ) => AsyncResult<
    void,
    AggregateNotFoundError<"Memo"> | DuplicationError<"Memo"> | UnexpectedError
  >;
  find: (
    identifier: MemoIdentifier
  ) => AsyncResult<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>;
  search: (criteria: Criteria) => AsyncResult<Memo[], UnexpectedError>;
  terminate: (
    identifier: MemoIdentifier
  ) => AsyncResult<void, AggregateNotFoundError<"Memo"> | UnexpectedError>;
}
