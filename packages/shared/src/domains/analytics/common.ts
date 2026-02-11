import z from "zod";
import { Result, ok, err } from "@shared/aspects/result";
import { validate, ValidationError, validationError } from "@shared/aspects/error";

export const dateKeySchema = z
  .string()
  .regex(/^\d{4}-\d{2}-\d{2}$/)
  .brand("DateKey");

export type DateKey = z.infer<typeof dateKeySchema>;

export const sessionKeySchema = z.string().uuid().brand("SessionKey");

export type SessionKey = z.infer<typeof sessionKeySchema>;

export const periodSchema = z.enum(["7d", "30d", "90d", "all"]).brand("Period");

export type Period = z.infer<typeof periodSchema>;

export const dateRangeSchema = z
  .object({
    start: z.date(),
    end: z.date(),
  })
  .refine((data) => data.end >= data.start, {
    message: "end must be >= start",
  })
  .brand("DateRange");

export type DateRange = z.infer<typeof dateRangeSchema>;

export const periodComparisonSchema = z
  .object({
    current: z.number(),
    previous: z.number(),
  })
  .brand("PeriodComparison");

export type PeriodComparison = z.infer<typeof periodComparisonSchema>;

export const rankedItemSchema = z
  .object({
    label: z.string(),
    value: z.number(),
    subLabel: z.string().optional(),
  })
  .brand("RankedItem");

export type RankedItem = z.infer<typeof rankedItemSchema>;

export const trendPointSchema = z
  .object({
    dateKey: dateKeySchema,
    value: z.number(),
  })
  .brand("TrendPoint");

export type TrendPoint = z.infer<typeof trendPointSchema>;

export const distributionSchema = z
  .object({
    label: z.string(),
    value: z.number(),
  })
  .brand("Distribution");

export type Distribution = z.infer<typeof distributionSchema>;

const JST_OFFSET_MS = 9 * 60 * 60 * 1000;
const MS_PER_DAY = 24 * 60 * 60 * 1000;

const PERIOD_DAYS: Record<string, number> = {
  "7d": 7,
  "30d": 30,
  "90d": 90,
};

const ALL_START_DATE = new Date("2020-01-01T00:00:00Z");

export const toJstDateKey = (date: Date): DateKey =>
  dateKeySchema.parse(
    new Date(date.getTime() + JST_OFFSET_MS).toISOString().slice(0, 10),
  );

export const resolveDateRange = (period: Period): DateRange => {
  const now = new Date();
  const days = PERIOD_DAYS[period];

  if (days === undefined) {
    return dateRangeSchema.parse({ start: ALL_START_DATE, end: now });
  }

  const start = new Date(now.getTime() - days * MS_PER_DAY);
  return dateRangeSchema.parse({ start, end: now });
};

export const resolvePreviousDateRange = (period: Period): DateRange => {
  const days = PERIOD_DAYS[period];

  if (days === undefined) {
    return dateRangeSchema.parse({
      start: ALL_START_DATE,
      end: ALL_START_DATE,
    });
  }

  const now = new Date();
  const currentStart = new Date(now.getTime() - days * MS_PER_DAY);
  const previousStart = new Date(currentStart.getTime() - days * MS_PER_DAY);
  return dateRangeSchema.parse({ start: previousStart, end: currentStart });
};

export const generateDateKeys = (range: DateRange): DateKey[] => {
  const keys: DateKey[] = [];
  const current = new Date(range.start);

  while (current <= range.end) {
    keys.push(toJstDateKey(current));
    current.setDate(current.getDate() + 1);
  }

  return keys;
};

export const calculateTrendPercentage = (
  current: number,
  previous: number,
): number => {
  if (previous === 0) {
    return current > 0 ? 100 : 0;
  }
  return Math.round(((current - previous) / previous) * 100);
};

export const validatePeriod = (
  candidate: string,
): Result<Period, ValidationError> => {
  const result = periodSchema.safeParse(candidate);
  if (result.success) {
    return ok(result.data);
  }
  return err(
    validationError(
      "period",
      result.error.issues[0]?.message ?? "Invalid period",
    ),
  );
};

export const validatePeriodComparison = (
  candidate: { current: number; previous: number },
): Result<PeriodComparison, ValidationError[]> =>
  validate(periodComparisonSchema, candidate);

export const validateTrendPoint = (
  candidate: { dateKey: string; value: number },
): Result<TrendPoint, ValidationError[]> =>
  validate(trendPointSchema, candidate);

export const validateRankedItem = (
  candidate: { label: string; value: number; subLabel?: string },
): Result<RankedItem, ValidationError[]> =>
  validate(rankedItemSchema, candidate);

export const validateDistribution = (
  candidate: { label: string; value: number },
): Result<Distribution, ValidationError[]> =>
  validate(distributionSchema, candidate);
