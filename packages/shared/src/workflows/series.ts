import {
  AggregateNotFoundError,
  DuplicationError,
  ValidationError,
} from "@/aspects/error";
import { Logger } from "@/aspects/logger";
import { AsyncResult, Result } from "@/aspects/result";
import { Series, SeriesIdentifier, UnvalidatedSeries } from "@/domains/series";
import {
  createSeriesCreatedEvent,
  createSeriesTerminatedEvent,
  createSeriesUpdatedEvent,
  SeriesCreatedEvent,
  SeriesTerminatedEvent,
  SeriesUpdatedEvent,
} from "@/domains/series/event";

type ValidateIdentifier = (
  identifier: string
) => Result<SeriesIdentifier, ValidationError>;

type FindSeries = (
  identifier: SeriesIdentifier
) => AsyncResult<Series, AggregateNotFoundError<"Series">>;

export type SeriesFindWorkflow = (
  identifier: string
) => AsyncResult<Series, ValidationError | AggregateNotFoundError<"Series">>;

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

type SearchSeries = (title: string) => AsyncResult<Series[], never>;

export type SeriesSearchWorkflow = (
  title: string
) => AsyncResult<Series[], never>;

export const createSeriesSearchWorkflow =
  (search: SearchSeries) =>
  (logger: Logger): SeriesSearchWorkflow =>
  (title: string) => {
    logger.info("SeriesSearchWorkflow started", { title });

    return search(title)
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
) => AsyncResult<
  void,
  AggregateNotFoundError<"Series"> | DuplicationError<"Series">
>;

type SeriesPersistedEvent = SeriesCreatedEvent | SeriesUpdatedEvent;

export type SeriesPersistWorkflow = (
  unvalidated: UnvalidatedSeries,
  isNew: boolean
) => AsyncResult<
  SeriesPersistedEvent,
  | AggregateNotFoundError<"Series">
  | DuplicationError<"Series">
  | ValidationError[]
>;

export const createSeriesPersistWorkflow =
  (validate: ValidateSeries) =>
  (persist: PersistSeries) =>
  (logger: Logger): SeriesPersistWorkflow =>
  (unvalidated: UnvalidatedSeries, isNew: boolean) => {
    logger.info("SeriesPersistWorkflow started", {
      identifier: unvalidated.identifier,
      isNew,
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
          .map(() =>
            isNew
              ? createSeriesCreatedEvent(series.identifier)
              : createSeriesUpdatedEvent(series.identifier)
          )
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
) => AsyncResult<void, AggregateNotFoundError<"Series">>;

export type SeriesTerminateWorkflow = (
  identifier: string
) => AsyncResult<
  SeriesTerminatedEvent,
  ValidationError | AggregateNotFoundError<"Series">
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
