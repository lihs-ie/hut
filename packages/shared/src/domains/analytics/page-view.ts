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
import {
  searchReferenceIdentifierSchema,
  equalSearchReferenceIdentifier,
} from "../search-token/reference";

export const deviceTypeSchema = z
  .enum(["desktop", "mobile", "tablet"])
  .brand("DeviceType");

export type DeviceType = z.infer<typeof deviceTypeSchema>;

export const referrerSchema = z
  .object({
    raw: z.string().nullable(),
  })
  .brand("Referrer");

export type Referrer = z.infer<typeof referrerSchema>;

export const pageViewIdentifierSchema = z
  .object({
    reference: searchReferenceIdentifierSchema,
    dateKey: dateKeySchema,
    sessionKey: sessionKeySchema,
  })
  .brand("PageViewIdentifier");

export type PageViewIdentifier = z.infer<typeof pageViewIdentifierSchema>;

export const equalPageViewIdentifier = (
  left: PageViewIdentifier,
  right: PageViewIdentifier,
): boolean =>
  equalSearchReferenceIdentifier(left.reference, right.reference) &&
  left.dateKey === right.dateKey &&
  left.sessionKey === right.sessionKey;

export const pageViewSchema = z
  .object({
    identifier: pageViewIdentifierSchema,
    referrer: referrerSchema,
    deviceType: deviceTypeSchema,
    createdAt: z.date(),
  })
  .brand("PageView");

export type PageView = z.infer<typeof pageViewSchema>;

export type UnvalidatedPageView = {
  identifier: {
    reference: { type: string; content: string };
    dateKey: string;
    sessionKey: string;
  };
  referrer: { raw: string | null };
  deviceType: string;
  createdAt: Date;
};

export const validatePageView = (
  candidate: UnvalidatedPageView,
): Result<PageView, ValidationError[]> => validate(pageViewSchema, candidate);

export const criteriaSchema = z
  .object({
    dateRange: dateRangeSchema.nullish(),
    reference: searchReferenceIdentifierSchema.nullish(),
    deviceType: deviceTypeSchema.nullish(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  dateRange?: { start: Date; end: Date } | null;
  reference?: { type: string; content: string } | null;
  deviceType?: string | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export type PageViewError =
  | ValidationError
  | AggregateNotFoundError<"PageView">
  | DuplicationError<"PageView">
  | UnexpectedError;

export interface PageViewRepository {
  find: (
    identifier: PageViewIdentifier,
  ) => AsyncResult<
    PageView,
    AggregateNotFoundError<"PageView"> | UnexpectedError
  >;

  search: (criteria: Criteria) => AsyncResult<PageView[], UnexpectedError>;

  persist: (
    pageView: PageView,
  ) => AsyncResult<void, DuplicationError<"PageView"> | UnexpectedError>;

  terminate: (
    identifier: PageViewIdentifier,
  ) => AsyncResult<void, AggregateNotFoundError<"PageView"> | UnexpectedError>;
}

const TABLET_PATTERN = /tablet|ipad/i;
const MOBILE_PATTERN = /mobile|android|iphone/i;

const DEVICE_TYPE_DESKTOP = deviceTypeSchema.parse("desktop");
const DEVICE_TYPE_TABLET = deviceTypeSchema.parse("tablet");
const DEVICE_TYPE_MOBILE = deviceTypeSchema.parse("mobile");

export const detectDeviceType = (userAgent: string | null): DeviceType => {
  if (!userAgent) return DEVICE_TYPE_DESKTOP;
  if (TABLET_PATTERN.test(userAgent)) return DEVICE_TYPE_TABLET;
  if (MOBILE_PATTERN.test(userAgent)) return DEVICE_TYPE_MOBILE;
  return DEVICE_TYPE_DESKTOP;
};

export const extractReferrerDomain = (referrer: Referrer): string => {
  if (!referrer.raw) return "Direct";

  try {
    const url = new URL(referrer.raw);
    return url.hostname;
  } catch {
    return "Direct";
  }
};
