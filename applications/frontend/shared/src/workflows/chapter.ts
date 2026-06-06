import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { Logger } from "@shared/aspects/logger";
import { AsyncResult, ok, Result } from "@shared/aspects/result";
import { Slug, ValidateSlug } from "@shared/domains/common";
import {
  Chapter,
  ChapterIdentifier,
  ChapterPersistedEvent,
  ChapterRepository,
  ChapterTerminatedEvent,
  createChapterPersistedEvent,
  createChapterTerminatedEvent,
  UnvalidatedChapter,
} from "@shared/domains/series/chapter";
import {
  addChapter,
  removeChapter,
  Series,
  UnvalidatedSeries,
} from "@shared/domains/series";
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
  ChapterTerminatedEvent,
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
        terminate(id)
          .tap(() => {
            logger.debug("Chapter terminated", { identifier: id });
          })
          .map(() => createChapterTerminatedEvent(id))
      )
      .tap((event) => {
        logger.info("ChapterTerminateWorkflow completed", { event });
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

type ChapterFilter = (
  chapter: Chapter,
) => Result<Chapter, AggregateNotFoundError<"Chapter">>;

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
  ChapterPersistedEvent,
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
          .map(() => createChapterPersistedEvent(chapter.identifier))
      )
      .tap((event) => {
        logger.info("ChapterPersistWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("ChapterPersistWorkflow failed", { error });
      });
  };

export const createChapterFindBySlugWorkflow =
  (validate: ValidateSlug) =>
  (logger: Logger) =>
  (findBySlug: FindChapterBySlug) =>
  (filter: ChapterFilter): ChapterFindBySlugWorkflow =>
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
      .andThen(filter)
      .tap((chapter) => {
        logger.info("ChapterFindBySlugWorkflow completed", {
          identifier: chapter.identifier,
        });
      })
      .tapError((error) => {
        logger.error("ChapterFindBySlugWorkflow failed", { error });
      });
  };

type FindSeriesBySlug = (
  slug: Slug,
) => AsyncResult<Series, ValidationError | AggregateNotFoundError<"Series"> | UnexpectedError>;

type PersistSeries = (
  unvalidated: UnvalidatedSeries,
) => AsyncResult<unknown, ValidationError[] | DuplicationError<"Series"> | AggregateNotFoundError<"Series"> | UnexpectedError>;

const toUnvalidatedSeries = (series: Series): UnvalidatedSeries => ({
  identifier: series.identifier,
  title: series.title,
  slug: series.slug,
  subTitle: series.subTitle ?? null,
  description: series.description,
  cover: series.cover ?? null,
  tags: series.tags,
  chapters: series.chapters,
  status: series.status,
  publishedAt: series.publishedAt ?? null,
  timeline: {
    createdAt: series.timeline.createdAt,
    updatedAt: new Date(),
  },
});

export type ChapterPersistWithSeriesWorkflow = (
  unvalidated: UnvalidatedChapter,
  seriesSlug: string,
) => AsyncResult<
  void,
  | ValidationError
  | ValidationError[]
  | DuplicationError<"Chapter">
  | DuplicationError<"Series">
  | AggregateNotFoundError<"Chapter">
  | AggregateNotFoundError<"Series">
  | UnexpectedError
>;

export const createChapterPersistWithSeriesWorkflow =
  (persistChapter: ChapterPersistWorkflow) =>
  (validateSlug: ValidateSlug) =>
  (findSeriesBySlug: FindSeriesBySlug) =>
  (persistSeries: PersistSeries) =>
  (logger: Logger): ChapterPersistWithSeriesWorkflow =>
  (unvalidated: UnvalidatedChapter, seriesSlug: string) => {
    logger.info("ChapterPersistWithSeriesWorkflow started", {
      chapterIdentifier: unvalidated.identifier,
      seriesSlug,
    });

    return persistChapter(unvalidated)
      .andThen((_event) =>
        validateSlug(seriesSlug)
          .toAsync()
          .andThen(findSeriesBySlug)
      )
      .andThen((series) => {
        const chapterIdentifier = unvalidated.identifier as ChapterIdentifier;
        if (series.chapters.includes(chapterIdentifier)) {
          return ok<void, never>(undefined).toAsync();
        }
        const updatedSeries = addChapter(series, chapterIdentifier);
        return persistSeries(toUnvalidatedSeries(updatedSeries)).map(() => undefined as void);
      })
      .tap(() => {
        logger.info("ChapterPersistWithSeriesWorkflow completed", {
          chapterIdentifier: unvalidated.identifier,
          seriesSlug,
        });
      })
      .tapError((error) => {
        logger.error("ChapterPersistWithSeriesWorkflow failed", { error });
      });
  };

export type ChapterTerminateWithSeriesWorkflow = (
  chapterIdentifier: string,
  seriesSlug: string,
) => AsyncResult<
  void,
  | ValidationError
  | ValidationError[]
  | AggregateNotFoundError<"Chapter">
  | AggregateNotFoundError<"Series">
  | DuplicationError<"Series">
  | UnexpectedError
>;

export const createChapterTerminateWithSeriesWorkflow =
  (terminateChapter: ChapterTerminateWorkflow) =>
  (validateSlug: ValidateSlug) =>
  (findSeriesBySlug: FindSeriesBySlug) =>
  (persistSeries: PersistSeries) =>
  (logger: Logger): ChapterTerminateWithSeriesWorkflow =>
  (chapterIdentifier: string, seriesSlug: string) => {
    logger.info("ChapterTerminateWithSeriesWorkflow started", {
      chapterIdentifier,
      seriesSlug,
    });

    return terminateChapter(chapterIdentifier)
      .andThen((_event) =>
        validateSlug(seriesSlug)
          .toAsync()
          .andThen(findSeriesBySlug)
      )
      .andThen((series) => {
        const updatedSeries = removeChapter(
          series,
          chapterIdentifier as ChapterIdentifier,
        );
        return persistSeries(toUnvalidatedSeries(updatedSeries)).map(() => undefined as void);
      })
      .tap(() => {
        logger.info("ChapterTerminateWithSeriesWorkflow completed", {
          chapterIdentifier,
          seriesSlug,
        });
      })
      .tapError((error) => {
        logger.error("ChapterTerminateWithSeriesWorkflow failed", { error });
      });
  };
