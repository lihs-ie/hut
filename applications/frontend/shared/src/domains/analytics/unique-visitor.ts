import z from "zod";
import { AsyncResult, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  validate,
  ValidationError,
} from "@shared/aspects/error";
import { dateKeySchema, sessionKeySchema, dateRangeSchema } from "./common";

export const uniqueVisitorIdentifierSchema = z
  .object({
    dateKey: dateKeySchema,
    sessionKey: sessionKeySchema,
  })
  .brand("UniqueVisitorIdentifier");

export type UniqueVisitorIdentifier = z.infer<
  typeof uniqueVisitorIdentifierSchema
>;

export const validateUniqueVisitorIdentifier = (
  candidate: { dateKey: string; sessionKey: string },
): Result<UniqueVisitorIdentifier, ValidationError[]> =>
  validate(uniqueVisitorIdentifierSchema, candidate);

export const equalUniqueVisitorIdentifier = (
  left: UniqueVisitorIdentifier,
  right: UniqueVisitorIdentifier,
): boolean =>
  left.dateKey === right.dateKey && left.sessionKey === right.sessionKey;

export const uniqueVisitorSchema = z
  .object({
    identifier: uniqueVisitorIdentifierSchema,
    createdAt: z.date(),
  })
  .brand("UniqueVisitor");

export type UniqueVisitor = z.infer<typeof uniqueVisitorSchema>;

export type UnvalidatedUniqueVisitor = {
  identifier: {
    dateKey: string;
    sessionKey: string;
  };
  createdAt: Date;
};

export const validateUniqueVisitor = (
  candidate: UnvalidatedUniqueVisitor,
): Result<UniqueVisitor, ValidationError[]> =>
  validate(uniqueVisitorSchema, candidate);

export const criteriaSchema = z
  .object({
    dateRange: dateRangeSchema.nullish(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  dateRange?: { start: Date; end: Date } | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export interface UniqueVisitorRepository {
  find: (
    identifier: UniqueVisitorIdentifier,
  ) => AsyncResult<
    UniqueVisitor,
    AggregateNotFoundError<"UniqueVisitor"> | UnexpectedError
  >;

  search: (criteria: Criteria) => AsyncResult<UniqueVisitor[], UnexpectedError>;

  persist: (
    visitor: UniqueVisitor,
  ) => AsyncResult<
    void,
    DuplicationError<"UniqueVisitor"> | UnexpectedError
  >;

  terminate: (
    identifier: UniqueVisitorIdentifier,
  ) => AsyncResult<
    void,
    AggregateNotFoundError<"UniqueVisitor"> | UnexpectedError
  >;
}
