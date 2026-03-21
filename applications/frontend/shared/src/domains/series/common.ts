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
import { chapterIdentifierSchema, ChapterIdentifier } from "./chapter";

export const seriesIdentifierSchema = z.ulid();

export type SeriesIdentifier = z.infer<typeof seriesIdentifierSchema>;

export const validateSeriesIdentifier = (
  candidate: string
): Result<SeriesIdentifier, ValidationError> => {
  const result = seriesIdentifierSchema.safeParse(candidate);

  if (result.success) {
    return ok(result.data);
  } else {
    return err(
      validationError(
        "SeriesIdentifier",
        `Invalid series identifier: ${candidate}`
      )
    );
  }
};

export const titleSchema = z.string().min(1).max(100);

export type Title = z.infer<typeof titleSchema>;

export const subTitleSchema = z.string().min(1).max(200).optional();

export type SubTitle = z.infer<typeof subTitleSchema>;

export const descriptionSchema = z.string().max(500).optional();

export type Description = z.infer<typeof descriptionSchema>;

export const cover = z.url();

export type Cover = z.infer<typeof cover>;

export type SeriesSlug = Slug;

export type ChapterSlug = Slug;

export const seriesSchema = z
  .object({
    identifier: seriesIdentifierSchema,
    title: titleSchema,
    slug: slugSchema,
    tags: z.array(tagIdentifierSchema),
    subTitle: subTitleSchema.nullable(),
    description: descriptionSchema,
    cover: cover.nullable(),
    chapters: z.array(chapterIdentifierSchema),
    status: publishStatusSchema,
    timeline: timelineSchema,
  })
  .brand("Series");

export type Series = z.infer<typeof seriesSchema>;

export type UnvalidatedSeries = {
  identifier: string;
  title: string;
  slug: string;
  tags: string[];
  subTitle: string | null;
  chapters: string[];
  status: string;
  description?: string;
  cover?: string | null;
  timeline?: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validateSeries = (
  candidate: UnvalidatedSeries
): Result<Series, ValidationError[]> => validate(seriesSchema, candidate);

export const addChapter = (
  series: Series,
  chapterIdentifier: ChapterIdentifier
): Series => {
  return {
    ...series,
    chapters: [...series.chapters, chapterIdentifier],
  };
};

export const removeChapter = (
  series: Series,
  chapterIdentifier: ChapterIdentifier
): Series => {
  return {
    ...series,
    chapters: series.chapters.filter((id) => id !== chapterIdentifier),
  };
};

export const criteriaSchema = z
  .object({
    slug: slugSchema.nullable(),
    tags: z.array(tagIdentifierSchema).nullable(),
    status: publishStatusSchema.nullable(),
    freeWord: z.string().min(1).max(100).nullable(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  slug: string | null;
  tags: string[] | null;
  status: string | null;
  freeWord: string | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

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
  findBySlug: (
    slug: SeriesSlug
  ) => AsyncResult<Series, AggregateNotFoundError<"Series"> | UnexpectedError>;
  ofIdentifiers: (
    identifiers: SeriesIdentifier[],
    throwOnMissing?: boolean
  ) => AsyncResult<
    Series[],
    UnexpectedError | AggregateNotFoundError<"Series">
  >;
  search: (criteria: Criteria) => AsyncResult<Series[], UnexpectedError>;
  terminate: (
    identifier: SeriesIdentifier
  ) => AsyncResult<void, AggregateNotFoundError<"Series"> | UnexpectedError>;
}
