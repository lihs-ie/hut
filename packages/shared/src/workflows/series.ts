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
  Criteria,
  Series,
  SeriesIdentifier,
  UnvalidatedCriteria,
  UnvalidatedSeries,
} from "@shared/domains/series";
import {
  createSeriesCreatedEvent,
  createSeriesTerminatedEvent,
  SeriesCreatedEvent,
  SeriesTerminatedEvent,
} from "@shared/domains/series/event";
import { Command } from "./common";

type ValidateIdentifier = (
  identifier: string
) => Result<SeriesIdentifier, ValidationError>;

type FindSeries = (
  identifier: SeriesIdentifier
) => AsyncResult<Series, AggregateNotFoundError<"Series"> | UnexpectedError>;

export type SeriesFindWorkflow = (
  identifier: string
) => AsyncResult<
  Series,
  ValidationError | AggregateNotFoundError<"Series"> | UnexpectedError
>;
export const createSeriesFindWorkflow =
  (validate: ValidateIdentifier) =>
  (find: FindSeries) =>
  (logger: Logger): SeriesFindWorkflow =>
  (identifier: string) => {
    logger.info("SeriesFindWorkflow started", { identifier });

    return validate(identifier)
      .toAsync()
      .tap((id) => {
        logger.debug("Validation passed", { identifier: id });
      })
      .tapError((error) => {
        logger.warn("Validation failed", { error });
      })
      .andThen(find)
      .tap((series) => {
        logger.info("SeriesFindWorkflow completed", {
          identifier: series.identifier,
        });
      })
      .tapError((error) => {
        logger.error("SeriesFindWorkflow failed", { error });
      });
  };

type SeriesFindBySlugCommand = Command<{ slug: string }>;

type SeriesFindBySlugWorkflow = (
  command: SeriesFindBySlugCommand
) => AsyncResult<
  Series,
  ValidationError | AggregateNotFoundError<"Series"> | UnexpectedError
>;

type FindBySLug = {
  (slug: Slug): AsyncResult<
    Series,
    AggregateNotFoundError<"Series"> | UnexpectedError
  >;
};

export const createSeriesFindBySlugWorkflow =
  (validate: ValidateSlug) =>
  (logger: Logger) =>
  (findBySlug: FindBySLug): SeriesFindBySlugWorkflow =>
  (command: SeriesFindBySlugCommand) => {
    logger.info("SeriesFindBySlugWorkflow started", {
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
      .tap((series) => {
        logger.info("SeriesFindBySlugWorkflow completed", {
          identifier: series.identifier,
        });
      })
      .tapError((error) => {
        logger.error("SeriesFindBySlugWorkflow failed", { error });
      });
  };

type SearchSeries = (
  criteria: Criteria
) => AsyncResult<Series[], UnexpectedError>;

type ValidateCriteria = (
  unvalidated: UnvalidatedCriteria
) => Result<Criteria, ValidationError[]>;

export type SeriesSearchWorkflow = (
  unvalidated: UnvalidatedCriteria
) => AsyncResult<Series[], ValidationError[] | UnexpectedError>;

export const createSeriesSearchWorkflow =
  (validateCriteria: ValidateCriteria) =>
  (search: SearchSeries) =>
  (logger: Logger): SeriesSearchWorkflow =>
  (unvalidated: UnvalidatedCriteria) => {
    logger.info("SeriesSearchWorkflow started", { unvalidated });

    return validateCriteria(unvalidated)
      .toAsync()
      .tap((criteria) => {
        logger.debug("Criteria validation passed", { criteria });
      })
      .tapError((errors) => {
        logger.warn("Criteria validation failed", { errors });
      })
      .andThen(search)
      .tap((seriesList) => {
        logger.info("SeriesSearchWorkflow completed", {
          resultCount: seriesList.length,
        });
      })
      .tapError((error) => {
        logger.error("SeriesSearchWorkflow failed", { error });
      });
  };

type ValidateSeries = (
  unvalidated: UnvalidatedSeries
) => Result<Series, ValidationError[]>;

type PersistSeries = (
  series: Series
) => AsyncResult<void, DuplicationError<"Series"> | UnexpectedError>;

type SeriesPersistedEvent = SeriesCreatedEvent;

export type SeriesPersistWorkflow = (
  unvalidated: UnvalidatedSeries
) => AsyncResult<
  SeriesPersistedEvent,
  | AggregateNotFoundError<"Series">
  | DuplicationError<"Series">
  | ValidationError[]
  | UnexpectedError
>;

export const createSeriesPersistWorkflow =
  (validate: ValidateSeries) =>
  (persist: PersistSeries) =>
  (logger: Logger): SeriesPersistWorkflow =>
  (unvalidated: UnvalidatedSeries) => {
    logger.info("SeriesPersistWorkflow started", {
      identifier: unvalidated.identifier,
    });

    return validate(unvalidated)
      .toAsync()
      .tap((series) => {
        logger.debug("Series validation passed", {
          identifier: series.identifier,
        });
      })
      .tapError((errors) => {
        logger.warn("Series validation failed", { errors });
      })
      .andThen((series) =>
        persist(series)
          .tap(() => {
            logger.debug("Series persisted", {
              identifier: series.identifier,
            });
          })
          .map(() => createSeriesCreatedEvent(series.identifier))
      )
      .tap((event) => {
        logger.info("SeriesPersistWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("SeriesPersistWorkflow failed", { error });
      });
  };

type TerminateSeries = (
  identifier: SeriesIdentifier
) => AsyncResult<void, AggregateNotFoundError<"Series"> | UnexpectedError>;

export type SeriesTerminateWorkflow = (
  identifier: string
) => AsyncResult<
  SeriesTerminatedEvent,
  ValidationError | AggregateNotFoundError<"Series"> | UnexpectedError
>;

export const createSeriesTerminateWorkflow =
  (validate: ValidateIdentifier) =>
  (terminate: TerminateSeries) =>
  (logger: Logger): SeriesTerminateWorkflow =>
  (identifier: string) => {
    logger.info("SeriesTerminateWorkflow started", { identifier });

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
            logger.debug("Series terminated", { identifier: id });
          })
          .map(() => createSeriesTerminatedEvent(id))
      )
      .tap((event) => {
        logger.info("SeriesTerminateWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("SeriesTerminateWorkflow failed", { error });
      });
  };
