import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { Logger } from "@shared/aspects/logger";
import { AsyncResult, Result } from "@shared/aspects/result";
import { Slug, ValidateSlug } from "@shared/domains/common";
import {
  Chapter,
  ChapterIdentifier,
  ChapterRepository,
  UnvalidatedChapter,
} from "@shared/domains/series/chapter";
import { Command } from "./common";

type ValidateChapterIdentifier = (
  identifier: string
) => Result<ChapterIdentifier, ValidationError>;

type TerminateChapter = (
  identifier: ChapterIdentifier
) => AsyncResult<void, AggregateNotFoundError<"Chapter"> | UnexpectedError>;

export type ChapterTerminateWorkflow = (
  identifier: string
) => AsyncResult<
  void,
  ValidationError | AggregateNotFoundError<"Chapter"> | UnexpectedError
>;

export const createChapterTerminateWorkflow =
  (validate: ValidateChapterIdentifier) =>
  (terminate: TerminateChapter) =>
  (logger: Logger): ChapterTerminateWorkflow =>
  (identifier: string) => {
    logger.info("ChapterTerminateWorkflow started", { identifier });

    return validate(identifier)
      .toAsync()
      .tap((id) => {
        logger.debug("Validation passed", { identifier: id });
      })
      .tapError((error) => {
        logger.warn("Validation failed", { error });
      })
      .andThen((id) =>
        terminate(id).tap(() => {
          logger.debug("Chapter terminated", { identifier: id });
        })
      )
      .tap(() => {
        logger.info("ChapterTerminateWorkflow completed", { identifier });
      })
      .tapError((error) => {
        logger.error("ChapterTerminateWorkflow failed", { error });
      });
  };

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

type ValidateChapter = (
  unvalidated: UnvalidatedChapter,
) => Result<Chapter, ValidationError[]>;

type PersistChapter = ChapterRepository["persist"];

export type ChapterPersistWorkflow = (
  unvalidated: UnvalidatedChapter,
) => AsyncResult<
  void,
  | ValidationError[]
  | DuplicationError<"Chapter">
  | AggregateNotFoundError<"Chapter">
  | UnexpectedError
>;

export const createChapterPersistWorkflow =
  (validate: ValidateChapter) =>
  (persist: PersistChapter) =>
  (logger: Logger): ChapterPersistWorkflow =>
  (unvalidated: UnvalidatedChapter) => {
    logger.info("ChapterPersistWorkflow started", {
      identifier: unvalidated.identifier,
    });

    return validate(unvalidated)
      .toAsync()
      .tap((chapter) => {
        logger.debug("Chapter validation passed", {
          identifier: chapter.identifier,
        });
      })
      .tapError((errors) => {
        logger.warn("Chapter validation failed", { errors });
      })
      .andThen((chapter) =>
        persist(chapter)
          .tap(() => {
            logger.debug("Chapter persisted", {
              identifier: chapter.identifier,
            });
          })
      )
      .tap(() => {
        logger.info("ChapterPersistWorkflow completed", {
          identifier: unvalidated.identifier,
        });
      })
      .tapError((error) => {
        logger.error("ChapterPersistWorkflow failed", { error });
      });
  };

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
