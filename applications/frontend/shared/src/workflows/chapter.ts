import {
  AggregateNotFoundError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { Logger } from "@shared/aspects/logger";
import { AsyncResult } from "@shared/aspects/result";
import { Slug, ValidateSlug } from "@shared/domains/common";
import { Chapter } from "@shared/domains/series/chapter";
import { Command } from "./common";

type ChapterFindBySlugCommand = Command<{ slug: string }>;

export type ChapterFindBySlugWorkflow = (
  command: ChapterFindBySlugCommand
) => AsyncResult<
  Chapter,
  ValidationError | AggregateNotFoundError<"Chapter"> | UnexpectedError
>;

type FindChapterBySlug = (
  slug: Slug
) => AsyncResult<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>;

export const createChapterFindBySlugWorkflow =
  (validate: ValidateSlug) =>
  (logger: Logger) =>
  (findBySlug: FindChapterBySlug): ChapterFindBySlugWorkflow =>
  (command: ChapterFindBySlugCommand) => {
    logger.info("ChapterFindBySlugWorkflow started", {
      slug: command.payload.slug,
    });

    return validate(command.payload.slug)
      .toAsync()
      .tap((slug) => {
        logger.debug("Validation passed", { slug });
      })
      .tapError((error) => {
        logger.warn("Validation failed", { error });
      })
      .andThen(findBySlug)
      .tap((chapter) => {
        logger.info("ChapterFindBySlugWorkflow completed", {
          identifier: chapter.identifier,
        });
      })
      .tapError((error) => {
        logger.error("ChapterFindBySlugWorkflow failed", { error });
      });
  };
