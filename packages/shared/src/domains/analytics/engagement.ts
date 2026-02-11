import z from "zod";
import { AsyncResult, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  UnexpectedError,
  validate,
  ValidationError,
} from "@shared/aspects/error";
import { dateKeySchema, sessionKeySchema, dateRangeSchema } from "./common";
import {
  searchReferenceIdentifierSchema,
  equalSearchReferenceIdentifier,
} from "../search-token/reference";

export const dwellTimeSchema = z
  .number()
  .int()
  .min(0)
  .max(86_400)
  .brand("DwellTime");

export type DwellTime = z.infer<typeof dwellTimeSchema>;

export const scrollDepthSchema = z
  .number()
  .int()
  .min(0)
  .max(100)
  .brand("ScrollDepth");

export type ScrollDepth = z.infer<typeof scrollDepthSchema>;

export const engagementRecordIdentifierSchema = z
  .object({
    reference: searchReferenceIdentifierSchema,
    dateKey: dateKeySchema,
    sessionKey: sessionKeySchema,
  })
  .brand("EngagementRecordIdentifier");

export type EngagementRecordIdentifier = z.infer<
  typeof engagementRecordIdentifierSchema
>;

export const equalEngagementRecordIdentifier = (
  left: EngagementRecordIdentifier,
  right: EngagementRecordIdentifier,
): boolean =>
  equalSearchReferenceIdentifier(left.reference, right.reference) &&
  left.dateKey === right.dateKey &&
  left.sessionKey === right.sessionKey;

export const engagementRecordSchema = z
  .object({
    identifier: engagementRecordIdentifierSchema,
    dwellTime: dwellTimeSchema,
    scrollDepth: scrollDepthSchema,
    createdAt: z.date(),
    updatedAt: z.date(),
  })
  .brand("EngagementRecord");

export type EngagementRecord = z.infer<typeof engagementRecordSchema>;

export type UnvalidatedEngagementRecord = {
  identifier: {
    reference: { type: string; content: string };
    dateKey: string;
    sessionKey: string;
  };
  dwellTime: number;
  scrollDepth: number;
  createdAt: Date;
  updatedAt: Date;
};

export const validateEngagementRecord = (
  candidate: UnvalidatedEngagementRecord,
): Result<EngagementRecord, ValidationError[]> =>
  validate(engagementRecordSchema, candidate);

export const criteriaSchema = z
  .object({
    dateRange: dateRangeSchema.nullish(),
    reference: searchReferenceIdentifierSchema.nullish(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  dateRange?: { start: Date; end: Date } | null;
  reference?: { type: string; content: string } | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export interface EngagementRecordRepository {
  find: (
    identifier: EngagementRecordIdentifier,
  ) => AsyncResult<
    EngagementRecord,
    AggregateNotFoundError<"EngagementRecord"> | UnexpectedError
  >;

  search: (
    criteria: Criteria,
  ) => AsyncResult<EngagementRecord[], UnexpectedError>;

  persist: (record: EngagementRecord) => AsyncResult<void, UnexpectedError>;

  terminate: (
    identifier: EngagementRecordIdentifier,
  ) => AsyncResult<
    void,
    AggregateNotFoundError<"EngagementRecord"> | UnexpectedError
  >;
}

export const formatDwellTime = (dwellTime: DwellTime): string => {
  const minutes = Math.floor(dwellTime / 60);
  const remainingSeconds = Math.floor(dwellTime % 60);
  return `${minutes}:${String(remainingSeconds).padStart(2, "0")}`;
};
