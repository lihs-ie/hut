import z from "zod";
import { ulid } from "ulid";
import { publishStatusSchema, Slug, slugSchema, timelineSchema } from "../common";
import { Event } from "../common/event";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  validate,
  validationError,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, ok, err, Result } from "@shared/aspects/result";
import { imageIdentifierSchema } from "../image/identifier";

export const chapterIdentifierSchema = z.ulid().brand("ChapterIdentifier");

export type ChapterIdentifier = z.infer<typeof chapterIdentifierSchema>;

export const chapterTitleSchema = z
  .string()
  .min(1, { message: "Chapter title must be at least 1 character long" })
  .max(100, { message: "Chapter title must be at most 100 characters long" })
  .brand("ChapterTitle");

export type ChapterTitle = z.infer<typeof chapterTitleSchema>;

export const contentSchema = z
  .string()
  .min(1, { message: "Chapter content must be at least 1 character long" })
  .brand("Content");

export type Content = z.infer<typeof contentSchema>;

export const chapterSchema = z
  .object({
    identifier: chapterIdentifierSchema,
    title: chapterTitleSchema,
    slug: slugSchema,
    content: contentSchema,
    images: z.array(imageIdentifierSchema),
    status: publishStatusSchema,
    timeline: timelineSchema,
  })
  .brand("Chapter");

export type Chapter = z.infer<typeof chapterSchema>;

export type UnvalidatedChapter = {
  identifier: string;
  title: string;
  slug: string;
  content: string;
  images: string[];
  status: string;
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validateChapterIdentifier = (
  candidate: string
): Result<ChapterIdentifier, ValidationError> => {
  const result = chapterIdentifierSchema.safeParse(candidate);

  if (result.success) {
    return ok(result.data);
  } else {
    return err(
      validationError(
        "ChapterIdentifier",
        `Invalid chapter identifier: ${candidate}`,
      )
    );
  }
};

export const validateChapter = (
  candidate: UnvalidatedChapter
): Result<Chapter, ValidationError[]> => validate(chapterSchema, candidate);

type ChapterPayload = {
  chapter: ChapterIdentifier;
};

export type ChapterPersistedEvent = Event<"chapter.persisted", ChapterPayload>;

export type ChapterTerminatedEvent = Event<"chapter.terminated", ChapterPayload>;

export const createChapterPersistedEvent = (
  identifier: ChapterIdentifier,
): ChapterPersistedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  type: "chapter.persisted",
  payload: {
    chapter: identifier,
  },
});

export const createChapterTerminatedEvent = (
  identifier: ChapterIdentifier,
): ChapterTerminatedEvent => ({
  identifier: ulid(),
  occurredAt: new Date(),
  type: "chapter.terminated",
  payload: {
    chapter: identifier,
  },
});

export type ChapterEvent = ChapterPersistedEvent | ChapterTerminatedEvent;

export interface ChapterRepository {
  find: (
    identifier: ChapterIdentifier
  ) => AsyncResult<
    Chapter,
    AggregateNotFoundError<"Chapter"> | UnexpectedError
  >;
  findBySlug: (
    slug: Slug
  ) => AsyncResult<
    Chapter,
    AggregateNotFoundError<"Chapter"> | UnexpectedError
  >;
  ofIdentifiers: (
    identifiers: ChapterIdentifier[],
    throwOnMissing?: boolean
  ) => AsyncResult<
    Chapter[],
    UnexpectedError | AggregateNotFoundError<"Chapter">
  >;
  persist: (
    chapter: Chapter
  ) => AsyncResult<void, UnexpectedError | DuplicationError<"Chapter">>;
  terminate: (
    identifier: ChapterIdentifier
  ) => AsyncResult<void, AggregateNotFoundError<"Chapter"> | UnexpectedError>;
}
