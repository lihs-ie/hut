import z from "zod";
import { Slug, slugSchema, timelineSchema } from "../common";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  validate,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, Result } from "@shared/aspects/result";

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
    timeline: timelineSchema,
  })
  .brand("Chapter");

export type Chapter = z.infer<typeof chapterSchema>;

export type UnvalidatedChapter = {
  identifier: string;
  title: string;
  slug: string;
  content: string;
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validateChapter = (
  candidate: UnvalidatedChapter
): Result<Chapter, ValidationError[]> => validate(chapterSchema, candidate);

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
  persist: (
    chapter: Chapter
  ) => AsyncResult<void, UnexpectedError | DuplicationError<"Chapter">>;
  terminate: (
    identifier: ChapterIdentifier
  ) => AsyncResult<void, AggregateNotFoundError<"Chapter"> | UnexpectedError>;
}
