import {
  DuplicationError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, Result, ok, combineAsync } from "@shared/aspects/result";
import { Logger } from "@shared/aspects/logger";
import { Command } from "@shared/workflows/common";
import {
  UniqueVisitor,
  UnvalidatedUniqueVisitor,
  Criteria as UniqueVisitorCriteria,
  validateCriteria,
} from "@shared/domains/analytics/unique-visitor";
import {
  Period,
  PeriodComparison,
  toJstDateKey,
  resolveDateRange,
  resolvePreviousDateRange,
  validatePeriodComparison,
} from "@shared/domains/analytics/common";
import {
  UniqueVisitorRecordedEvent,
  createUniqueVisitorRecordedEvent,
} from "@shared/domains/analytics/event";

type RecordUniqueVisitorPayload = {
  sessionKey: string;
};

type ValidateUniqueVisitor = (
  unvalidated: UnvalidatedUniqueVisitor,
) => Result<UniqueVisitor, ValidationError[]>;

type PersistUniqueVisitor = (
  visitor: UniqueVisitor,
) => AsyncResult<void, DuplicationError<"UniqueVisitor"> | UnexpectedError>;

export type RecordUniqueVisitorWorkflow = (
  command: Command<RecordUniqueVisitorPayload>,
) => AsyncResult<
  UniqueVisitorRecordedEvent,
  DuplicationError<"UniqueVisitor"> | ValidationError[] | UnexpectedError
>;

export const createRecordUniqueVisitorWorkflow =
  (validate: ValidateUniqueVisitor) =>
  (persist: PersistUniqueVisitor) =>
  (logger: Logger): RecordUniqueVisitorWorkflow =>
  (command: Command<RecordUniqueVisitorPayload>) => {
    logger.info("RecordUniqueVisitorWorkflow started", {
      sessionKey: command.payload.sessionKey,
    });

    const dateKey = toJstDateKey(command.now);

    return validate({
      identifier: { dateKey, sessionKey: command.payload.sessionKey },
      createdAt: command.now,
    })
      .toAsync()
      .tap((visitor) => {
        logger.debug("UniqueVisitor validation passed", {
          identifier: visitor.identifier,
        });
      })
      .tapError((errors) => {
        logger.warn("UniqueVisitor validation failed", { errors });
      })
      .andThen((visitor) =>
        persist(visitor)
          .tap(() => {
            logger.debug("UniqueVisitor persisted", {
              identifier: visitor.identifier,
            });
          })
          .andThen(() => ok(visitor)),
      )
      .map((visitor) => createUniqueVisitorRecordedEvent(visitor, command.now))
      .tap((event) => {
        logger.info("RecordUniqueVisitorWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("RecordUniqueVisitorWorkflow failed", { error });
      });
  };

type ValidatePeriod = (
  candidate: string,
) => Result<Period, ValidationError>;

type SearchUniqueVisitors = (
  criteria: UniqueVisitorCriteria,
) => AsyncResult<UniqueVisitor[], UnexpectedError>;

export type GetUniqueVisitorsWorkflow = (
  command: Command<{ period: string }>,
) => AsyncResult<PeriodComparison, ValidationError | UnexpectedError>;

export const createGetUniqueVisitorsWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchUniqueVisitors) =>
  (logger: Logger): GetUniqueVisitorsWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetUniqueVisitorsWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .andThen((period) => {
        const currentRange = resolveDateRange(period);
        const previousRange = resolvePreviousDateRange(period);
        const currentCriteria = validateCriteria({ dateRange: currentRange }).unwrap();
        const previousCriteria = validateCriteria({ dateRange: previousRange }).unwrap();
        return combineAsync([
          search(currentCriteria),
          search(previousCriteria),
        ]);
      })
      .map(([currentResults, previousResults]) =>
        validatePeriodComparison({
          current: currentResults.length,
          previous: previousResults.length,
        }).unwrap(),
      )
      .tap((result) => {
        logger.info("GetUniqueVisitorsWorkflow completed", {
          current: result.current,
          previous: result.previous,
        });
      })
      .tapError((error) => {
        logger.error("GetUniqueVisitorsWorkflow failed", { error });
      });
  };
