import z from "zod";
import { timelineSchema } from "../common/date";
import { AsyncResult, err, ok, Result } from "@/aspects/result";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  ValidationError,
  validationErrors,
} from "@/aspects/error";

export const seriesIdentifierSchema = z.ulid();

export type SeriesIdentifier = z.infer<typeof seriesIdentifierSchema>;

export const titleSchema = z.string().min(1).max(100);

export type Title = z.infer<typeof titleSchema>;

export const descriptionSchema = z.string().max(500).optional();

export type Description = z.infer<typeof descriptionSchema>;

export const cover = z.url();

export type Cover = z.infer<typeof cover>;

export const pageSchema = z
  .object({
    title: z.string().min(1).max(100),
    content: z.string().min(1),
    timeline: timelineSchema,
  })
  .brand("Page");

export type Page = z.infer<typeof pageSchema>;

export type UnvalidatedPage = {
  title: string;
  content: string;
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validatePage = (
  candidate: UnvalidatedPage
): Result<Page, ValidationError[]> => {
  const errors = validationErrors(pageSchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(pageSchema.parse(candidate));
};

export const seriesSchema = z
  .object({
    identifier: seriesIdentifierSchema,
    title: titleSchema,
    description: descriptionSchema,
    cover: cover.nullable(),
    pages: z.array(pageSchema),
    timeline: timelineSchema,
  })
  .brand("Series");

export type Series = z.infer<typeof seriesSchema>;

export type UnvalidatedSeries = {
  identifier: string;
  title: string;
  pages: UnvalidatedPage[];
  description?: string;
  cover?: string | null;
};

export const validateSeries = (
  candidate: UnvalidatedSeries
): Result<Series, ValidationError[]> => {
  const errors = validationErrors(seriesSchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(seriesSchema.parse(candidate));
};

export type SeriesError =
  | ValidationError
  | AggregateNotFoundError<"Series">
  | DuplicationError<"Series">;

export interface SeriesRepository {
  persist: (
    series: Series
  ) => AsyncResult<
    void,
    | AggregateNotFoundError<"Series">
    | DuplicationError<"Series">
    | UnexpectedError
  >;
  find: (
    identifier: SeriesIdentifier
  ) => AsyncResult<Series, AggregateNotFoundError<"Series"> | UnexpectedError>;
  search: (title: string) => AsyncResult<Series[], UnexpectedError>;
  terminate: (
    identifier: SeriesIdentifier
  ) => AsyncResult<void, AggregateNotFoundError<"Series"> | UnexpectedError>;
}
